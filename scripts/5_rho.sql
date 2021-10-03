################################################################################
# scripts/5_rho.sql                                                            #
# Unpivots table tmp from long to wide/tidy and calculates                     #
#  the air density, wind direction, and wind speed                             #
# Took ~8.5 hours to run on the researcher's config (https://bit.ly/3ChCBAP)   #
################################################################################

# Set the engine's buffer size to 50GB. Adjust this to however much RAM you have to spare
SET GLOBAL innodb_buffer_pool_size = 50 * 1024 * 1024 * 1024;

# Make sure table nc is deleted if it exists
DROP TABLE IF EXISTS nc;

# Define the function that calculates air density (rho) from hurs, ps, and tas
DELIMITER $$
DROP FUNCTION IF EXISTS CalcRho;
CREATE FUNCTION CalcRho (
	hurs FLOAT,
	ps FLOAT,
	tas FLOAT
)
RETURNS FLOAT
DETERMINISTIC
BEGIN
	# Declare the variables used in the function
	DECLARE es0 FLOAT;
	DECLARE Rd FLOAT;
	DECLARE Rv FLOAT;
	DECLARE K FLOAT;
	DECLARE pol FLOAT;
	DECLARE esw FLOAT;
	DECLARE pv FLOAT;
	DECLARE pd FLOAT;
	DECLARE rho FLOAT;
	# Set input constants
	# Reference saturation vapor pressure at 0°C in mbar
	SET es0 = 6.1078;
		# Specific gas constant for dry air, in J/(kg·K)
	SET Rd = 287.058;
	# Specific gas constant for water vapor, in J/(kg·K)
	SET Rv = 461.495;
	# Kelvin to Celsius
	SET K = 273.15;
	# Set the polynomial approximation for the saturation vapor pressure at tas
	# Adapted from FORTRAN to SQL from the function ESW(T) found at https://icoads.noaa.gov/software/other/profs by Herman Wobus
	# Requires tas in °C
	SET pol = 0.99999683 +
	 (tas - K) * (-0.90826951E-02 +
	  (tas - K) * (0.78736169E-04 +
	   (tas - K) * (-0.61117958E-06 +
		(tas - K) * (0.43884187E-08 +
		 (tas - K) * (-0.29883885E-10 +
		  (tas - K) * (0.21874425E-12 +
		   (tas - K) * (-0.17892321E-14 +
			(tas - K) * (0.11112018E-16 +
			 (tas - K) * (-0.30994571E-19)))))))));
	# Set output variables
	# Saturation vapor pressure at tas, in mbar
	SET esw = es0 / POWER(pol, 8);
	# Partial pressure of water vapor, in mbar
	SET pv = esw * hurs / 100;
	# Partial pressure of dry air, in mbar
	SET pd = ps / 100 - pv;
	# Total air density in kg/m3. Requires tas in K
	SET rho = ((pd / (Rd * tas)) + (pv / (Rv * tas))) * 100;
	# Return the air density
	RETURN(rho);
END $$
DELIMITER ;

# Define the function that calculates the direction that the wind is coming from
DELIMITER $$
DROP FUNCTION IF EXISTS CalcWndDir;
CREATE FUNCTION CalcWndDir (
	uas FLOAT,
	vas FLOAT
)
RETURNS FLOAT
DETERMINISTIC
BEGIN
	# Declare the variables used in the function
	DECLARE WndDir FLOAT;
  # Calculate the wind direction
	SET WndDir = MOD(180 + (180 / PI()) * ATAN(uas, vas), 360);
	# Return the wind direction
	RETURN(WndDir);
END $$
DELIMITER ;

# Define the function that calculates the wind speed (in m/s)
DELIMITER $$
DROP FUNCTION IF EXISTS CalcWndSpd;
CREATE FUNCTION CalcWndSpd (
	uas FLOAT,
	vas FLOAT
)
RETURNS FLOAT
DETERMINISTIC
BEGIN
	# Declare the variables used in the function
	DECLARE WndSpd FLOAT;
	# Calculate the wind speed (in m/s)
	SET WndSpd = SQRT(POWER(uas, 2) + POWER(vas, 2));
	# Return the wind speed (in m/s)
	RETURN(WndSpd);
END $$
DELIMITER ;

# Create the nc table from the tmp table
CREATE TABLE nc (id INT NOT NULL AUTO_INCREMENT, PRIMARY KEY (id))
AS
WITH
	hurs AS (SELECT obs, icao, exp, val FROM tmp WHERE var = 'hurs'),
	ps   AS (SELECT obs, icao, exp, val FROM tmp WHERE var = 'ps'),
	tas  AS (SELECT obs, icao, exp, val FROM tmp WHERE var = 'tas'),
	uas  AS (SELECT obs, icao, exp, val FROM tmp WHERE var = 'uas'),
	vas  AS (SELECT obs, icao, exp, val FROM tmp WHERE var = 'vas')
SELECT
	hurs.obs AS obs,
	hurs.icao AS icao,
    hurs.exp AS exp,
    hurs.val AS hurs,
    ps.val AS ps,
    tas.val AS tas,
    uas.val AS uas,
    vas.val AS vas,
    CalcRho(hurs.val, ps.val, tas.val) AS rho,
    CalcWndDir(uas.val, vas.val) AS WndDir,
    CalcWndSpd(uas.val, vas.val) AS WndSpd
FROM hurs
INNER JOIN ps  ON hurs.exp = ps.exp  AND hurs.icao = ps.icao  AND hurs.obs = ps.obs
INNER JOIN tas ON hurs.exp = tas.exp AND hurs.icao = tas.icao AND hurs.obs = tas.obs
INNER JOIN uas ON hurs.exp = uas.exp AND hurs.icao = uas.icao AND hurs.obs = uas.obs
INNER JOIN vas ON hurs.exp = vas.exp AND hurs.icao = vas.icao AND hurs.obs = vas.obs;
#ORDER BY obs, icao, exp;

# Create a composite index on the resulting table
CREATE INDEX idx ON nc (exp, icao, obs);

# Delete the tmp table to save disk space
#DROP TABLE IF EXISTS tmp;