******************************************************************************************************************
* Analyse Haringtest scores 2016/17
* Code repliceert verklaringsmodel zoals gebruikt in 
* Onderzoek 1 (28 juli 2017): Gaat de AD haringtest om meer dan de haring? 
* URL: https://surfdrive.surf.nl/files/index.php/s/gCPganKc8lbRMZZ
* Onderzoek 2 (15 november 2017) : Gaat de AD haringtest om meer dan de haring? Een update
* URL: https://www.tilburguniversity.edu/sites/default/files/download/haringtest_vollaard_def_1.pdf
******************************************************************************************************************
* Bron gegevens: Algemeen Dagblad. 
* Definitie variabelen weergegeven in labels van variabelen
* Twee variabelen zijn aan de gegevens van Algemeen Dagblad toegevoegd:
* (1) k30: afstand verkooppunt tot Rotterdam meer dan 30 km
* (2) A_d: verkooppunt is afnemer van leverancier waar belangenconflict speelt
* Noot: ad (2) Zoals beschreven in onderzoek 2, heb ik zo goed mogelijk gereconstrueerd welk verkooppunt wel en niet
* werd beleverd door de bewuste firma. Alle leveranciers van haring hadden hiervoor de relevante gegevens uit hun 
* verkoopadministratie met mij gedeeld - op een na, de bewuste firma. Daarnaast had ik voor 244 van de 292 
* verkooppunten directe bevestiging over wie de haring levert.
*******************************************************************************************************************

*******************************************************************************************************************
* Inladen gegevens Haringtest 2016/17
*******************************************************************************************************************

use "...\scores_haringtest_2016_2017.dta"

*******************************************************************************************************************
**** ONDERZOEK 1 (jul 2017) ***************************************************************************************
*******************************************************************************************************************

* Tabel 1: afzonderlijke berekeningen o.b.v. geschatte coefficienten verklaringsmodel (zie hieronder)

* Appendix: verklaringsmodel

* Model 1: uitsluitend kenmerken haring	
reg eindcijfer gewicht i.temp_cat i.vet_cat i.prijs_cat vers_num i.micro_num i.rijping_num i.schoonmaak_num j2017

* Model 2: inclusief invloed vestigingsplaats
reg eindcijfer gewicht i.temp_cat i.vet_cat i.prijs_cat vers_num i.micro_num i.rijping_num i.schoonmaak_num k30 j2017

*******************************************************************************************************************
**** ONDERZOEK 2 (nov 2017) ***************************************************************************************
*******************************************************************************************************************

* Figuur 1: gemiddelde score van verkooppunten op de haringtest 2016/17, naar leverancier

bys A_d: summ eindcijfer
bys A_d: summ eindcijfer if (micro_num==1|micro_num==2)

* Noot: definitie variabele eindcijfer: eindcijfer haringtest + 7,5

* Tabel 1: afzonderlijke berekeningen o.b.v. geschatte coefficienten verklaringsmodel (zie hieronder)

* Appendix: verklaringsmodel

reg eindcijfer gewicht i.temp_cat i.vet_cat vers_num i.micro_num i.rijping_num i.schoonmaak_num k30 buitentop10 j2017

* Noot: de in Tabel A1 van Onderzoek 2 vermelde constante hoort bij verklaringsmodel met variable 'top10';
* bij opnemen van 'buitentop10' ipv 'top10' -- waarbij buitentop10=1-top10 -- blijft alles identiek, behalve de constante
