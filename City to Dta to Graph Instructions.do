clear all
di "`c(pwd)'"
local place = "Ghent"
cd "/Users/Philip/Documents/NU Econ PhD/Inside Airbnb Raw Data/`place'/"
local filepath = "`c(pwd)'"
di "`c(pwd)'"
local files : dir "`filepath'" files "*.xlsx"
di `"`files'"'


tempfile master // Generate temporary save file to store data in
save `master', replace empty


foreach x of local files {
    di "`x'" // Display file name

	//Import each file
	qui: import excel using "`x'", firstrow clear // Import excel file

//	qui: gen filename = subinstr("`x'", ".xlsx", "", .)	// Generate id variable (same as file name but without extension)

	// Append each file to masterfile
	append using `master', force
	save `master', replace
}

gen Month = substr(last_scraped , 6,2)                                                                       
gen Year = substr(last_scraped , 1,4) 


//fix off-set months
//replace Month = "04" if last_scrape =="2019-05-01"
//replace Month = "12"  if last_scrape =="2020-01-01"
//replace Year = "2019"  if last_scrape =="2020-01-01"
//
//replace Month = "12"  if last_scrape =="2020-01-09"
//replace Year = "2019"  if last_scrape =="2020-01-09"
// 
//replace Month = "02"  if last_scrape =="2020-03-01"
//
//replace Month = "04"  if last_scrape =="2020-05-01"
//replace Month = "04"  if last_scrape =="2020-05-02"
     
	 
//VICTORIA BC FIX
//replace Month = "04" if last_scraped =="2020-05-01" | last_scraped =="2020-05-02" | last_scraped =="2020-05-03" | last_scraped =="2020-05-04"
//replace Month_Long  = "april" if last_scraped =="2020-05-01" | last_scraped =="2020-05-02" | last_scraped =="2020-05-03" | last_scraped =="2020-05-04"
//replace Monthcodes   = 4 if last_scraped =="2020-05-01" | last_scraped =="2020-05-02" | last_scraped =="2020-05-03" | last_scraped =="2020-05-04"

//replace Month = "05" if last_scraped =="2020-06-04" | last_scraped =="2020-06-05" | last_scraped =="2020-06-06" 
//replace Monthcodes  = 5 if last_scraped =="2020-06-04" | last_scraped =="2020-06-05" | last_scraped =="2020-06-06" 
//replace Month_Long   = "may" if last_scraped =="2020-06-04" | last_scraped =="2020-06-05" | last_scraped =="2020-06-06" 



gen 	Month_Long = "sept" if  Month=="09"
replace Month_Long = "oct" if   Month=="10"
replace Month_Long = "nov" if   Month=="11"
replace Month_Long = "dec" if   Month=="12"
replace Month_Long = "jan" if   Month=="01"
replace Month_Long = "feb" if   Month=="02"
replace Month_Long = "mar" if   Month=="03"
replace Month_Long = "april" if Month=="04"
replace Month_Long = "may" if   Month=="05"
replace Month_Long = "june" if  Month=="06"
replace Month_Long = "july" if  Month=="07"
replace Month_Long = "aug" if   Month=="08"



gen Group = (Year=="2020"| (Year=="2019"& (Month_Long=="sept" | Month_Long=="oct" | Month_Long=="nov" | Month_Long=="dec" ))) & !missing(Year)


bys host_id Month Group: gen My_listing_count = _N


// verifies Group treatment assignment 
//tab filename group

gen superhost = (host_is_superhost =="t") & !missing(host_is_superhost )




label define monthorder 1 "sept" 2 "oct" 3 "nov" 4 "dec" 5 "jan" 6 "feb" 7 "mar" 8 "april" 9 "may" 10 "june" 11 "july" 12 "aug"

encode Month , gen(Monthcodes) label(monthorder)
label define Group 0 "2018-2019" 1 "2019-2020"
label values Group Group
gen post = (Monthcodes == 7 | Monthcodes == 8 | Monthcodes == 9 | Monthcodes == 10 |Monthcodes == 11 |Monthcodes == 12  ) & !missing(Monthcodes )

bys Group Month: egen total = count(id)




gen 	ordvar=1 if  Month_Long=="sept"
replace ordvar=2 if  Month_Long=="oct"
replace ordvar=3 if  Month_Long=="nov"
replace ordvar=4 if  Month_Long=="dec"
replace ordvar=5 if  Month_Long=="jan"
replace ordvar=6 if  Month_Long=="feb"
replace ordvar=7 if  Month_Long=="mar"
replace ordvar=8 if  Month_Long=="april"
replace ordvar=9 if  Month_Long=="may"
replace ordvar=10 if Month_Long=="june"
replace ordvar=11 if Month_Long=="july"
replace ordvar=12 if Month_Long=="aug"

local date: display %td_mon_DD date(c(current_date), "DMY")
local date_string = subinstr(trim("`date'"), " ","", .)

graph dot total    , over(Group) over(Month_Long, sort(ordvar )) asyvars ytitle("Count") title("`place' Total Listings Number") vertical
graph export "/Users/Philip/Documents/NU Econ PhD/Inside Airbnb Raw Data/`place'/`place'GraphTotal`date_string'.jpg", as(jpg) name("Graph") quality(100)


save "`place'`date_string'.dta", replace





