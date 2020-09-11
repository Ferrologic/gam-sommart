## Filer
Dataset för sommartävlingen består av 10 csv filer, Station_1.csv till Station_10.csv, en för varje mätstation. 

[Station_1.csv](Station_1.csv)  
[Station_2.csv](Station_2.csv)  
[Station_3.csv](Station_3.csv)  
[Station_4.csv](Station_4.csv)  
[Station_5.csv](Station_5.csv)  
[Station_6.csv](Station_6.csv)  
[Station_7.csv](Station_7.csv)  
[Station_8.csv](Station_8.csv)  
[Station_9.csv](Station_9.csv)  
[Station_10.csv](Station_10.csv)  

Samt vädervariablerna för valideringsdata:
[Väder_valideringsperiod.csv](Väder_valideringsperiod.csv)

## Variabel beskrivning

* Datum - Datum för Konsumtion och väder mätning. 2013-01-01 till och med 2016-12-31.
* Konsumtion - Total energikonsumtion (kWh) för Datum.
* Temperatur - Genomsnittlig utomhustemperatur i Celsius för Datum.
* Nederbördsmängd - Total nederbördsmängd för Datum uppmätt i mm.
* Vindriktning - Genomsnittlig vindriktning i grader för Datum.
* Vindhastighet - Genomsnittlig vindhastighet (m/s) för Datum.
* Lufttryck - Genomsnittligt lufttryck (Pascal) för Datum omräknat till ett värde som representerar trycket vid havsytans nivå.
* Daggpunktstemperatur - Genomsnittlig daggpunktstemperatur för Datum. Daggpunkten är ett mått på hur mycket vattenånga atmosfären innehåller. Om temperaturen sjunker till daggpunkten blir luften mättad med vattenånga och den relativa luftfuktigheten blir 100 %. Från mätningar av temperatur och relativ fuktighet beräknas daggpunkten.
* K2_regression - Prognos av energikonsumtion beräknad med K2s segmented regression modell. Referensperiod för alla 10 dataset var 2014-01-01 till och med 2014-12-31. Detta kan användas för att se om din modell ser ut att prestera bättre än nuvarande modell i K2.

All väderdata är nedladdat från SMHI (https://www.smhi.se/data/meteorologi/ladda-ner-meteorologiska-observationer#param=airtemperatureInstant,stations=all). Energikonsumtionen har normaliserats till samma skala för alla stationer för att en genomsnittlig beräkning av RMSD ska vara möjlig. Detta gjordes genom att skala energikonsumtion med K2 prognostiserat värde vid -5 °C. Utan denna normalisering domineras genomsnittliga RMSD av stationer med högre energikonsumtion. Valideringsperioden 2017-01-01 till och med 2017-06-26 är exkluderad från tillhandahållen data för att minska risken för övertränade modeller. Valideringsdata kommer offentliggöras efter tävlingens avlsut 1 september. 

