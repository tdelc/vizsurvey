<!--
Visite guidée — version française. Même principe que help_fr.md.

Une section = une étape, dans l'ordre du fichier.
  tab:    onglet à ouvrir (facultatif)
  target: sélecteur CSS de l'élément à entourer (facultatif -> bulle centrée)
  ##      titre de la bulle, le reste est du markdown
  {xxx}   valeur des données courantes, voir tour_values() dans mod_tour.R
          {survey} {n_rows} {n_vars} {n_intvwr} {n_waves}
          {wave} {filter} {n_outliers} {var_intvwr} {var_wave}

Si la cible n'existe pas à l'écran, la bulle s'affiche au centre sans
encadrement : l'étape reste lisible.
-->

# 01-bienvenue
tab: tab_summary
## Welkom bij vizsurvey
Deze tutorial leidt u in enkele stappen door de interface, aan de hand van 
de enquête die u hebt geladen: **{survey}**, oftewel {n_rows} interviews en 
{n_vars} geanalyseerde variabelen.

vizsurvey trekt geen conclusies voor u: het berekent afwijkingen en brengt 
deze onder de aandacht om u te helpen bij uw analyse van de enquête.

Gebruik **Volgende**, de pijltjestoetsen of **Esc** om af te sluiten.

# 02-source
target: #source-path_survey
## De enquête kiezen
U moet eerst de map en vervolgens de enquête kiezen. De statistieken
worden vooraf voorbereid door `prepa_survey()` en opgeslagen in een bestand
`global.rds`.

Als u van enquête wisselt, worden alle tabbladen onmiddellijk opnieuw geladen.

# 03-filtres
target: #filters-config_wave
## De golf en het filter
Deze twee selectors sturen **de rest van de applicatie** aan. Je bevindt je
momenteel op de golf **{wave}** en het filter **{filter}**.

Dit onderzoek telt {n_waves} golven, gedefinieerd door de variabele {var_wave}.
Het filter beperkt de analyse tot een subpopulatie: handig om te controleren
of een afwijking niet louter het gevolg is van een geografisch gebied.

# 04-seuils
target: #filters-button_parms
## De detectiedrempels
De drempels bepalen vanaf welke afwijking een vakje rood wordt,
en hoeveel geldige regels of waarden er nodig zijn om een
variabele te analyseren.

Er bestaat geen statistisch onderbouwde drempel: het is aan uw team om deze
door middel van trial and error in te stellen. Verlaag de drempel om meer 
gevallen te zien, verhoog deze om alleen de meest opvallende gevallen 
te behouden.

# 05-resume
tab: tab_summary
target: #summary-card_intvwr
## Waar te beginnen
Het eerste tabblad geeft drie cijfermatige aanknopingspunten. Voor de huidige 
golf en het huidige filter overschrijden {n_outliers} combinaties van 
interviewer × variabele de drempels.

Elke kaart heeft een knop die het bijbehorende tabblad opent.

# 06-vague
tab: tab_wave
target: #wave-card_cat
## Consistentie tussen de golven
Hier wordt de geselecteerde golf vergeleken met een of meer andere golven:
er wordt een variabele weergegeven als ten minste één element van de verdeling 
is veranderd (volgens de gedefinieerde drempelwaarde). Er is een tabel voor 
categorische variabelen en een tabel voor continue variabelen.

Typische voorbeelden zijn een modaliteit die verschijnt of verdwijnt, een 
sprong in ontbrekende waarden of een verandering van eenheid.

Klik op een regel: de grafiek aan de rechterkant toont de ontwikkeling, 
golf voor golf.

# 07-enqueteur
tab: tab_intvwr
target: #intvwr-card_synthesis
## Consistentie tussen interviewers
Eén rij per interviewer, gesorteerd op afwijkingsscore. De score is afkomstig
van een Isolation Forest: hoe zeldzamer de combinatie van indicatoren, hoe
hoger de rang.

Het onderzoek {survey} telt momenteel {n_intvwr} interviewers,
geïdentificeerd door de variabele {var_intvwr}. Klik op een regel om
de details te openen: interviews, sessies, variabelen, verdelingen.

# 08-croise
tab: tab_intvwr_variable
target: #intvwr_variable-cross_ranking
## De kruistabel enquêteur(ster) × variabele
De tabel kruist elke enquêteur(ster) met elke variabele uit de enquête.
Voor elke kruising wordt het verschil (chi²-indicator) gemeten tussen de
verdeling van de variabele voor deze enquêteur en die voor
de rest van de populatie.

De continue variabelen worden eerst omgezet in vijf kwantielen
(+ ontbrekende waarden) voordat de chi²-afstand wordt berekend.

De verschillen worden in aflopende volgorde gerangschikt om in de eerste plaats
de meest opvallende gevallen te bekijken. Klik op een vakje om de 
vergeleken verdelingen onderaan de pagina te bekijken.

# 09-croise2
tab: tab_intvwr_variable
target: #intvwr_variable-heatmap
## De heatmap
Eén rij per enquêteur, één kolom per variabele; een vakje is gekleurd
wanneer het verschil de drempel overschrijdt.

Er zijn drie regels om de kaart te interpreteren: een op zichzelf staand 
vakje is niet per se een probleem, een rode **rij** duidt op terugkerend 
gedrag, een rode **kolom** duidt op een variabele die waarschijnlijk moeilijk 
te begrijpen is.

Klik op een vakje om de vergelijkende verdelingen onderaan de pagina te 
bekijken.

# 10-données
tab: tab_data
target: #data-card_data
## De ruwe gegevens
Deze tabel geeft u toegang tot alle ruwe gegevens van het onderzoek, voor
alle rondes en zonder filters. U kunt indien nodig kolommen toevoegen
en filteren op basis van uw behoeften.

# 10-dict
tab: tab_dict
target: #dict-dict_table
## Het gegevenswoordenboek
Deze tabel geeft u toegang tot alle labels van variabelen en antwoordopties.
U kunt naar behoefte filteren om de
informatie over de resultaten te vinden die in het dashboard zijn gemarkeerd.

Er verschijnt automatisch een gefilterde tabel in de tabbladen ‘Enquêteur’
en ‘Kruistabel’ als de geselecteerde variabele in het woordenboek staat.

# 11-archive
target: #archive-add_button
## Een spoor achterlaten
Als je een geval hebt gecontroleerd, noteer dit dan. Het archief bewaart de 
datum, de gebruiker, de enquête, de enquêteur en de betreffende variabele.

Noteer ook valse meldingen: zo voorkom je dat de volgende persoon
hetzelfde onderzoek opnieuw moet doen.

# 12-aide
tab: tab_summary
target: #summary-card_survey .btn-link
## De helpfunctie van elke kaart
Dit kleine **?** staat op alle kaarten van de app. Het legt uit
wat de kaart weergeeft, wat de kolommen betekenen en hoe je deze 
moet interpreteren.
