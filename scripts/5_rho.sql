################################################################################
# scripts/6_rho.sql                                                            #
# Calculates the air density variable from hurs, ps, and tas                   #
#  Took ~4.5 hours to run on the researcher's config (https://bit.ly/3ChCBAP)  #
#   of which ~3.5 calculating rho and ~1 indexing the resulting table          #
################################################################################

# Set the engine's buffer size to 45GB
SET GLOBAL innodb_buffer_pool_size = 45 * 1024 * 1024 * 1024;

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

# Make sure table rho is deleted if it exists
DROP TABLE IF EXISTS rho;

# Create the table rho with an ID column plus other columns populated from the hurs, ps, and tas columns, and a final column val which is rho calculated from hurs/ps/tas
# Took ~3.55 hours on the researcher's config
CREATE TABLE rho (id INT NOT NULL AUTO_INCREMENT, PRIMARY KEY (id))
AS
SELECT
	hurs.obs AS obs,
	hurs.icao AS icao,
    hurs.exp AS exp,
    CalcRho(hurs.val, ps.val, tas.val) AS val
FROM hurs, ps, tas
WHERE
	hurs.id = ps.id
	AND
	hurs.id = tas.id;

# Create a composite index on the resulting table
CREATE INDEX idx ON rho (exp, icao, obs);