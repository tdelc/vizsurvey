<!--
Contextuele hulp van de cards — Nederlandse versie.

Vertaling van help_en.md: raak nooit de regels "# sleutel" aan, ze moeten
identiek blijven van de ene taal tot de andere. Een sleutel die hier ontbreekt
valt automatisch terug op het Engels.
-->

# wave-card_presence
## Variabelen die in een golf ontbreken
Deze tabel verschijnt alleen als een variabele **aanwezig is in de ene golf en
afwezig in een andere**, binnen de vergeleken golven.

- Elke kolom is een golf, elke cel toont de variabelen van die golf.
- ✔ de variabele bevat waarden, ✖ de variabele is leeg of afwezig.

Een variabele die verdwijnt is vaak normaal (wijziging van de vragenlijst,
nieuwe routing). Waar het om gaat, is nagaan of elke verdwijning gewild is,
vóór het lezen van de volgende tabellen: een variabele die in een golf
ontbreekt, kan niet vergeleken worden.

# wave-card_cat
## Categorische variabelen die bewogen hebben
Eén rij per categorische variabele waarvan de verdeling veranderd is tussen de
golf die in het linkerpaneel geselecteerd is en de vergelijkingsgolven.

- Eén kolomblok per golf, met `missing` (aandeel ontbrekende waarden) en
  `Nmod` (aantal waargenomen categorieën).
- Een cel wordt rood wanneer de variatie tussen golven de drempel
  *Minimaal variatiepercentage* overschrijdt (knop **Detectiedrempels**).
- Variabelen berekend op te weinig rijen of te weinig geldige waarden worden
  genegeerd (drempels *Minimaal aantal rijen* en *Minimaal aantal geldige
  waarden*).

Klik op een rij om de grafiek rechts weer te geven. Typische gevallen: een
categorie die verdwijnt, een sprong in ontbrekende waarden, een hercodering die
op één enkele golf is toegepast.

# wave-card_cat_detail
## Verdeling tussen de golven
Aandeel van elke categorie, golf per golf, voor de variabele die in de tabel
links geselecteerd is.

- Ontbrekende waarden verschijnen onder de categorie `.NA`.
- De kleuren liggen vast per categorie: eenzelfde categorie behoudt haar kleur
  van golf tot golf.
- Boven 15 categorieën wordt de grafiek niet getekend (onleesbaar).

Een breuk in de kleurbanden tussen twee golven bevestigt wat de tabel
signaleerde; stabiele banden wijzen op een vals alarm.

# wave-card_num
## Numerieke variabelen die bewogen hebben
Eén rij per numerieke variabele waarvan het niveau veranderd is tussen de golf
die in het linkerpaneel geselecteerd is en de vergelijkingsgolven.

- Eén kolomblok per golf, met `missing` (aandeel ontbrekende waarden) en
  `median` (mediaan).
- Een cel wordt rood wanneer de variatie tussen golven de drempel
  *Minimaal variatiepercentage* overschrijdt (knop **Detectiedrempels**).
- De mediaan wordt verkozen boven het gemiddelde, omdat ze minder gevoelig is
  voor extreme waarden.

Klik op een rij om de grafiek rechts weer te geven. Een mediaan die met een
ronde factor vermenigvuldigd of gedeeld is, wijst doorgaans op een wijziging
van eenheid of schaal eerder dan op een echte evolutie.

# wave-card_num_detail
## Dichtheid tussen de golven
Dichtheid van de variabele die in de tabel links geselecteerd is, één curve per
golf.

- De grafiek wordt afgekapt op het 1e en 99e percentiel, zodat enkele extreme
  waarden de curven niet platdrukken.
- Overlappende curven wijzen op een stabiele verdeling; een verschoven of
  samengedrukte curve bevestigt de breuk die de tabel signaleerde.

# intvwr-card_indics
## Keuze van de indicatoren
Indicatoren die gebruikt worden in de tabel *Synthese* en in de score.

Een indicator uitvinken verwijdert hem uit de tabel **en** uit de berekening
van de score. Nuttig wanneer een indicator geen betekenis heeft voor de
enquête, of om na te gaan of de rangschikking niet op één enkele indicator
berust.

# intvwr-card_synthesis
## Synthese per enquêteur
Eén rij per enquêteur, voor de golf en het filter die in het linkerpaneel
geselecteerd zijn.

- `RANK` rangschikt de enquêteurs volgens de anomaliescore (Isolation
  Forest): hoe zeldzamer de combinatie van indicatoren, hoe hoger de rang.
- `NB_INTV` is het aantal interviews, `INDEX_H` de homogeniteit van de
  antwoorden, `MAX_CHI2` de grootste afwijking waargenomen op een variabele.
- De indicatoren in `DURATION_` komen uit de timers, die in `PC_` komen
  uit Blaise.
- Een rode cel overschrijdt het gemiddelde van haar kolom met meer dan 2
  standaardafwijkingen.
- Enquêteurs onder de drempel *Minimaal aantal rijen* worden niet
  weergegeven.

De rang stuurt het onderzoek, hij bewijst niets: klik op een rij om het detail
te openen in de tabbladen eronder.

# intvwr-card_distrib
## Detail per indicator
Eén histogram per indicator, over alle weergegeven enquêteurs.

De verticale rode lijn markeert de waarde van de enquêteur die in de tabel
*Synthese* geselecteerd is. Ze toont of hij werkelijk geïsoleerd is, of gewoon
aan de rand van een brede verdeling zit — wat de rang alleen niet vertelt.

# intvwr-card_intv
## Interviews van de enquêteur
Alle interviews van de geselecteerde enquêteur, gereconstrueerd op basis van de
timers.

- `DURATION_INTV` is de totale duur, `DURATION_INTER_INTV` het interval met
  het vorige interview.
- Een 🔴 signaleert een vlag: te kort interview, nachtwerk, meerdere
  sessies, of een negatief interval (interviews die elkaar overlappen).

Klik op een interview om zijn sessies eronder te openen.

# intvwr-card_ssn
## Sessies van het interview
Een interview kan opgedeeld zijn in meerdere sessies (onderbroken en daarna
hervat).

- Eén rij per sessie, met haar begin, haar duur en het interval met de
  vorige.
- `CHECK_SCTN` toont één punt per sectie van de vragenlijst: 🟢 de sectie
  werd in deze sessie ingevuld, 🔴 dat gebeurde niet.

Veel zeer korte sessies, of secties die in wanorde zijn ingevuld, verdienen
een blik.

# intvwr-card_details
## Detail van het interview
Ruwe timergegevens voor het geselecteerde interview: één rij per item van de
vragenlijst, met zijn tijdstempel en zijn duur.

Dit is het fijnste beschikbare niveau. Het dient om een hogerop vastgestelde
anomalie te bevestigen, bijvoorbeeld een reeks items beantwoord in enkele
seconden.

# intvwr-card_var
## Variabelen van de enquêteur
Variabelen van de geselecteerde enquêteur, gerangschikt volgens afwijking
tegenover de andere enquêteurs van dezelfde golf en hetzelfde filter.

- `chi2` meet de afstand tussen de verdeling van de enquêteur en de
  referentieverdeling.
- `standard` is die afstand genormaliseerd per variabele: zij wordt
  vergeleken met de drempel *Minimale chi²-afstand*.
- Numerieke variabelen worden vooraf opgedeeld in 5 klassen: ze worden dus
  vergeleken zoals categorische variabelen.

Klik op een variabele om haar verdeling rechts weer te geven.

# intvwr-card_var_distrib
## Vergelijking van de verdelingen
Verdeling van de geselecteerde variabele voor de enquêteur (in donker)
tegenover het geheel van de andere enquêteurs (in grijs).

- Categorische variabele: aandeel van elke categorie, het weergegeven
  percentage is dat van de enquêteur.
- De tabel ernaast geeft de gebruikelijke statistieken voor de enquêteur en
  de rest van de interviews (eventueel gefilterd).

Lees de vorm, niet alleen de afwijking: een categorie die nooit gebruikt
wordt, of systematisch gebruikt wordt, zegt meer dan een klein verschil
gespreid over alles.

# intvwr-card_var_mods
## Verdeling van de categorieën
Voor elke categorie van de geselecteerde variabele, de verdeling van haar
aandeel **onder alle enquêteurs**, met een rode lijn op de waarde van de
enquêteur.

Deze kaart beantwoordt een vraag die de vorige niet behandelt: is deze
enquêteur geïsoleerd op deze categorie, of zitten veel anderen op hetzelfde
niveau?

# intvwr-card_geo
## Locatie van de interviews
Verdeling van de interviews van de geselecteerde enquêteur volgens de
geografische variabele opgegeven in `config.txt` (`var_info_geo`).

Een enquêteur die een ongebruikelijke zone bestrijkt, of één enkele kleine
zone, kan elders waargenomen afwijkingen verklaren: de respondenten zijn
simpelweg niet dezelfde.

# intvwr-card_stat
## Aantal interviews per golf
Aantal interviews van de geselecteerde enquêteur, per golf en per waarde
van het filter.

Deze kaart wordt niet gefilterd door het linkerpaneel: ze toont de hele
geschiedenis. Ze laat toe de ervaring en het huidige werk van een enquêteur
te analyseren.

# intvwr_variable-card_cross
## Afwijkingen enquêteur × variabele
Eén rij per paar enquêteur / variabele waarvan de afwijking de drempel
*Minimale chi²-afstand* overschrijdt.

- `chi2` is de afstand tussen de verdeling van de enquêteur en de
  referentieverdeling van de golf en het filter.
- `standard` is die afstand genormaliseerd per variabele, wat de variabelen
  onderling vergelijkbaar maakt.
- Numerieke variabelen worden vooraf opgedeeld in 5 klassen: de hele tabel
  leest dus op dezelfde manier.

De tabel is gesorteerd volgens dalende afwijking: dit is het startpunt wanneer
men niet weet waar te beginnen. Klik op een rij om de verdelingen onderaan de
pagina weer te geven.

# intvwr_variable-heatmap
## Kaart van de anomalieën
Eén rij per enquêteur, één kolom per variabele. Een cel is gekleurd wanneer de
enquêteur op die variabele afwijkt van de anderen.

- *Keuze van rijen en kolommen* zoomt in op de risicorijen, de risicokolommen,
  of enkel de cellen boven de drempel.
- *Keuze van de rangschikking* herordent rijen en kolommen om gelijkaardige
  profielen te groeperen (seriatie); `None` behoudt de alfabetische volgorde.
- Beweeg over een cel om de onderliggende waarden te zien, klik erop om de
  verdelingen onderaan de pagina te openen.

Een geïsoleerde cel is geen probleem. Een rode rij signaleert een terugkerend
gedrag, een rode kolom wijst op een fragiele variabele eerder dan op de
enquêteurs.

# intvwr_variable-card_intvwr_ranking
## Rangschikking van de enquêteurs
Enquêteurs gerangschikt volgens anomaliescore, berekend op hun afwijkingen
over alle variabelen.

- `score` komt uit een Isolation Forest: hoe atypischer het
  afwijkingsprofiel, hoe hoger de score.
- `N_outliers` telt de variabelen boven de detectiedrempel.
- `Nrow` is het aantal interviews, nuttig om een score gebouwd op weinig
  waarnemingen te relativeren.

Selecteer een enquêteur om zijn variabelen rechts op te lijsten.

# intvwr_variable-card_variable_listing
## Variabelen van de geselecteerde enquêteur
Variabelen van de links geselecteerde enquêteur, met hun afwijking (`diff`),
gesorteerd volgens dalende absolute waarde.

Deze lijst geeft aan welke variabelen de score dragen, en of ze een thema
delen (zelfde sectie van de vragenlijst, zelfde routing) — wat eerder naar een
procedurele dan een individuele oorzaak wijst.

# intvwr_variable-card_variable_ranking
## Rangschikking van de variabelen
Zelfde principe als de rangschikking van de enquêteurs, bekeken vanuit de
variabelen.

- `score` is hoog wanneer de afwijkingen van deze variabele ongelijk
  verdeeld zijn over de enquêteurs.
- `N_outliers` telt de enquêteurs boven de detectiedrempel.

Een variabele bovenaan deze tabel wijst vaak op een probleem met de
vragenlijst (dubbelzinnige formulering, routing) eerder dan op een enquêteur.
Selecteer ze om de betrokken enquêteurs rechts op te lijsten.

# intvwr_variable-card_intvwr_listing
## Enquêteurs van de geselecteerde variabele
Enquêteurs die afwijken op de links geselecteerde variabele, met hun
afwijking (`diff`), gesorteerd volgens dalende absolute waarde.

Als één of twee enquêteurs eruit springen, is de oorzaak waarschijnlijk
individueel. Als het er veel zijn, bekijk dan de variabele zelf, of een
subpopulatie-effect (probeer het filter van het linkerpaneel).

# intvwr_variable-card_distrib
## Vergelijking van de verdelingen
Verdeling van de geselecteerde variabele voor de geselecteerde enquêteur (in
donker) tegenover het geheel van de anderen (in grijs).

- Aandeel van elke categorie, het weergegeven percentage is
  dat van de enquêteur.
- Een numerieke variabele wordt vóór de analyse omgezet in een categorische
  per kwintiel.
- De tabel ernaast geeft de gebruikelijke statistieken voor de enquêteur.

Deze kaart wordt gevuld door op een cel van de heatmap of op een rij van de
tabellen erboven te klikken.

# intvwr_variable-card_distrib_mods
## Verdeling van de categorieën
Voor elke categorie van de geselecteerde variabele, de verdeling van haar
aandeel onder alle enquêteurs, met een rode lijn op de waarde van de
geselecteerde enquêteur.

Ze laat toe na te gaan of de enquêteur werkelijk geïsoleerd is op een
categorie, of dat het hele team zich over een brede waaier spreidt.

# data-card_data
## Ruwe gegevens
De gegevens van de enquête zoals ze werden voorbereid, zonder ander filter
dan de variabelenselectie erboven.

Nuttig om een waarde te controleren die in een analyse werd gezien, of om te
begrijpen hoe een variabele gecodeerd is. De zoekvakken bovenaan elke kolom
filteren de rijen; ontbrekende waarden worden weergegeven als `NA`.

# dict-card_dict
## Woordenboek van de variabelen
Labels en beschrijvingen van de variabelen, afkomstig uit het
woordenboekbestand dat bij het opstarten werd meegegeven.

Dit is de plaats om de exacte formulering van een vraag na te kijken alvorens
te besluiten over een afwijking: een verschil tussen golven kan verklaard
worden door een wijziging van label of routing.

# archive-card_archive
## Archief van de observaties
Commentaren geregistreerd vanuit de interface met de knop ** aan archief 
toevoegen**.

Elke rij bewaart de datum, de gebruiker, de enquête, de enquêteur en de
betrokken variabele, evenals het commentaar. Het bestand wordt gedeeld door
alle personen die hetzelfde archiefpad gebruiken.

Noteer wat u hebt gecontroleerd, met inbegrip van de valse alarmen: dat
vermijdt dat de volgende persoon hetzelfde onderzoek overdoet.

# summary-card_survey
## De geladen enquête
Identiteit van de enquête die in het linkerpaneel geselecteerd is.

- *Voorbereid op* is de datum van de laatste `prepa_survey()`: alles wat u in
  de applicatie ziet, dateert van dat moment, niet van de gegevens die nog
  verzameld worden.
- *Golf*, *Filter* en *Enquêteur* herinneren eraan welke variabelen die
  rollen spelen, zoals opgegeven in `config.txt`. Twee variabelen gescheiden
  door een schuine streep wijzen op een uitsplitsing op twee niveaus.
- *Timers* geeft aan of de audit trail beschikbaar is; zonder hem blijven de
  details van de interviews en de sessies leeg.

# summary-card_waves
## Interviews per golf
Aantal interviews van elke golf, over de hele enquête — deze grafiek wordt
niet beïnvloed door de selectie van het linkerpaneel.

Wanneer de golf twee niveaus heeft, wordt elke balk opgedeeld volgens het
tweede niveau (de kwartalen van een jaar, bijvoorbeeld). Een golf die veel
kleiner is dan de andere is normaal tijdens de verzameling, maar verdient
controle eens die is afgerond.

# summary-card_intvwr
## Waar te beginnen aan de kant van de enquêteurs
Aantal enquêteurs met minstens één afwijking boven de detectiedrempels, voor
de golf en het filter die in het linkerpaneel geselecteerd zijn.

De lijst geeft de vijf enquêteurs met het grootste aantal betrokken
variabelen. Open het tabblad *Enquêteur* voor de volledige rangschikking, de
auditindicatoren en het detail van de interviews.

# summary-card_wave
## Waar te beginnen aan de kant van de golven
Aantal variabelen waarvan het niveau bewogen heeft tussen de golven, voor het
filter dat in het linkerpaneel geselecteerd is.

De regel is die van het tabblad *Golf* (variatie van een indicator vergeleken
met de drempel *Minimaal variatiepercentage*), hier toegepast op alle golven
tegelijk. Het tabblad *Golf* laat daarentegen toe de te vergelijken golven te
kiezen: zijn lijst kan dus korter zijn.

# summary-card_cross
## Waar te beginnen aan de kant van de kruising
Aantal paren enquêteur × variabele boven de detectiedrempels, voor de golf en
het filter die in het linkerpaneel geselecteerd zijn.

De lijst geeft de vijf variabelen die bij het grootste aantal enquêteurs
gesignaleerd zijn — vaak het teken van een fragiele variabele eerder dan van
een enquêteursprobleem. Open het tabblad *Kruising* voor de heatmap en de
rangschikkingen.
