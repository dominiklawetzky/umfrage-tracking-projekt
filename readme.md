# Umfrage-Tracking-Tool 📈

## Hinweise
**Achtung: Dies ist eine Beta-Version.**

Die Daten aktualisieren sich nicht automatisch. Aktueller Stand ist 17. Juli 2021. 
(Leider gibt es keine zugänglichen APIs der einzelnen Institute. Daher müssen die Daten aufwendig händisch geladen und formatiert werden.)

Die Shiny-App stellt Umfrage-Daten von Infratest-Dimap, Allensbach und der Forschungsgruppe Wahlen für (mindestens) die letzten zehn Jahre dar. In der _Übersicht_ können die Unmfrage-Daten betrachtet werden; dafür stehen mehere Filter – z. B. nach Umfrage-Institut – zur Verfügung. Ebensfalls sind die Plots interaktiv. Unter dem Tab _Trendanalyse_ können aktuell die Umfrage-Daten zu einer Partei und eines Instituts geglättet dargestellt werden.

## FAQ

**Wie hole ich das meiste aus den Plots heraus?**
Dafür können zunächst die Filter-Tools an der rechten Seite bzw. oben verwendet werden. Dort kannst du bspw. Parteien oder Daten eines Umfrage-Instituts ausblenden. Außerdem kannst du zwei Marker für die vergangenen zwei Bundestagswahlen anzeigen lassen.

<p align="center">
<img src="HTML/Screenshots/Filter.png" alt="alt text" width="300px">
  </p>

Darüber hinaus sind die Plots interaktiv. Sobald du mit deiner Maus über den Plot fährst oder ihn anklickst, erscheint in der oberen rechten Ecke die [Plotly](https://plotly.com)-Toolbar.


<p align="center">
<img src="HTML/Screenshots/Plotly-Toolbar.png" alt="alt text" width="400px">
  </p>


Die Funktionen der Toolbar lassen sich wie folgt zusammenfassen (v. l. n. r.):
- Plot als Bild herunterladen
- In einen Bereich reinzoomen
- Den Plot entlang der Achsen bewegen
- Zentriert vergrößern
- Zentriert verkleinern
- Automatische Skalierung (ursprüngliche Ansicht)
- Achsen zurücksetzen (ursprüngliche Achsenabschnitte)
- (Keinen Nutzen für diesen Plot)
- Detaillierte Datendarstellung des jeweilig nächsten Datenpunkts
- Detaillierte Datendarstellung aller Datenpunkte zum jeweiligen x-Achsenabschnitt (insb. zum Vergleich)

**Woher stammen die Daten?**
Die Daten stammen von den offiziellen Websites der Umfrage-Instititute und im Fall der Forschungsgruppe Wahlen von der Seite [wahlrecht.de](https://www.wahlrecht.de/). Von dort übertrage ich die Daten teils händisch und mit dem praktischen _R_-Addon [Datapasta](https://milesmcbain.github.io/datapasta/). 

**Welche Programme wurden verwendet?**
Das Umfrage-Tracking-Tool basiert auf [Shiny](https://shiny.rstudio.com). Dahinter steckt _R_ – eine Programmiersprache, die vor allem im Bereich Statistik und Data Science verwendet wird.

Die wichtigsten verwendeten Tool sind:
- R Studio
- R Shiny
- Shiny Server
- Plotly
- Datapasta

Eine Liste aller verwendeten Pakete findet sich in der [Präambel](https://github.com/dominiklawetzky/umfrage-tracking-projekt/blob/main/preamble.R):

```R
packages <- c("ggplot2", "readxl", "dplyr", "tidyr", "knitr", "shiny")
```

**Ich habe eine Idee, wie das Umfrage-Tracking-Tool noch besser wird. Was soll ich tun?**
Du kannst [hier](https://github.com/dominiklawetzky/umfrage-tracking-projekt/issues) ein _Issue_ eröffnen. 

Da ich dieses Projekt in meiner Freizeit voranbringe, kann ich jedoch nicht versprechen, dass jede Feature-Request zügig umgesetzt wird. Wenn du selbst anpacken möchtest, freue ich mich. Bitte teste deine Änderungen gründlich, bevor du die Pull Request machst.

## Datenquellen
- [Infratest Dimap](https://www.infratest-dimap.de/umfragen-analysen/bundesweit/sonntagsfrage/)
- [Allensbach](https://www.ifd-allensbach.de/studien-und-berichte/sonntagsfrage/gesamt.html)
- [Forschungsgruppe Wahlen](https://www.wahlrecht.de/umfragen/politbarometer.htm)
