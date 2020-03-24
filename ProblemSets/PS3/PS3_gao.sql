	
	CREATE TABLE florida(

            "policyID" INTEGER,
	    "statecode" CHAR,
            "county" CHAR,
	    "eq_site_limit" DECIMAL,
	    "hu_site_limit" DECIMAL,
	    "fl_site_limit" INTEGER,
	    "fr_site_limit" DECIMAL,
	    "tiv_2011" DECIMAL,
	    "tiv_2012" DECIMAL,
	    "eq_site_deductible" DECIMAL, 
	    "hu_site_deductible" DECIMAL,
	    "fl_site_deductible" DECIMAL,
	    "fr_site_deductible" DECIMAL, 
	    "point_latitude" DECIMAL, 
	    "point_longitude" DECIMAL, 
	    "line" CHAR, 
	    "construction" CHAR, 
	    "point_granularity" INTEGER
	    );
	.mode csv
	.import "FL_insurance_sample.csv" florida;
	
	

        --print first 10 rows of the data set
	SELECT * FROM florida LIMIT 10;
	
	--list county variables
	SELECT county, COUNT(*) FROM florida GROUP BY county;
	
	--calculate the average property appreciation from 2011 to 2012
	SELECT AVG(tiv_2012 - tiv_2011) FROM florida;
	
	--show the frequency of each type of construction material
	SELECT construction, COUNT(*) FROM florida GROUP BY construction;


