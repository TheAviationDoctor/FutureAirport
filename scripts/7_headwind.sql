# Define the function that calculates headwind speed
DELIMITER $$
DROP FUNCTION IF EXISTS CalcHdwSpd;
CREATE FUNCTION CalcHdwSpd (
	hdg FLOAT,
	WndDir FLOAT,
	WndSpd FLOAT
)
RETURNS FLOAT
DETERMINISTIC
BEGIN
	# Declare the variables used in the function
	DECLARE WndDir FLOAT;
	DECLARE WndSpd FLOAT;
	# Calculate the headwind speed (in m/s)
	SET HdwSpd = COS(ABS(hdg - WndDir) * PI() / 180)
	# Return the headwind speed (in m/s)
	RETURN(HdwSpd);
END $$
DELIMITER ;

# Create the to table from the nc table
CREATE TABLE to (id INT NOT NULL AUTO_INCREMENT, PRIMARY KEY (id))
AS
WITH
	rwys AS (SELECT icao, SUBSTRING(rwy, 3, 2) AS hdg, MAX(toda) FROM pop WHERE traffic > 1000000 GROUP BY icao, hdg)
SELECT
	nc.obs AS obs,
	nc.icao AS icao,
	nc.exp AS exp,
	rwys.hdg AS hdg,
	nc.rho AS rho,
	CalcWndSpd(nc.WndDir, nc.WndDir) AS HdwSpd
FROM nc
RIGHT JOIN rwys ON nc.icao = rwys.icao;