-- Question 1(b)
-- List all stores in the dataset and number of records' count.
SELECT store COLLATE utf8mb4_bin AS store,
 	   COUNT(*) AS n
 FROM  shopper_profile
GROUP BY 1;

-- Rename "Osim" to standardize name "OSIM".
UPDATE shopper_profile
SET store = 'OSIM'
WHERE store = 'Osim';

-- Question 1(c)
-- Change the data type of each variables.
ALTER TABLE shopper_profile 
-- Reduce the length of the characters as 
-- mall generally do not have long names.
CHANGE `mall` `mall` VARCHAR(50) NULL COMMENT '',
-- Reduce the length of the characters as 
-- store generally do not have long names.
CHANGE `store` `store` VARCHAR(50) NULL COMMENT '',
-- Reduce the length of the characters as 
-- citizenship generally do not have long names.
CHANGE `citizenship` `citizenship` VARCHAR(30) 
	NULL COMMENT '',
-- Change data type of age from smallint to tinyint as 
-- it is impossible to live until 255.
CHANGE `age` `age` tinyint NOT NULL COMMENT '',
-- Reduce the length of the characters as 
-- gender generally do not have long names.
CHANGE `gender` `gender` VARCHAR(10) NULL COMMENT '',
-- Reduce the length of the characters as 
-- race generally do not have long names.
CHANGE `race` `race` VARCHAR(20) NULL COMMENT '';
