# FSM_Exam - Wine Dataset Analysis

## Descrizione del Progetto

Questo progetto universitario presenta un'analisi statistica completa del dataset `wine` utilizzando modelli grafici probabilistici. L'obiettivo principale è studiare le relazioni tra le diverse caratteristiche chimiche dei vini, suddivisi per cultivar (v1, v2, v3), e sviluppare modelli predittivi per la variabile target `Clri` (intensità del colore).

## Contenuti dell'Analisi

### 1. Analisi Esplorativa dei Dati
- Statistiche descrittive dell'intera popolazione e per singola cultivar
- Visualizzazioni grafiche:
  - Density plots
  - Boxplots
  - Scatter plot matrices
  - Heatmap delle correlazioni

### 2. Modelli Grafici Indiretti (Undirected Graphs)
- Calcolo delle matrici di concentrazione e correlazioni parziali
- Stima di grafi indiretti mediante:
  - Algoritmi stepwise (Forward, Backward, Both) con criteri AIC e BIC
  - Graphical LASSO con diverse penalizzazioni (ρ)
- Analisi separate per popolazione totale e singole cultivar

### 3. Modelli Grafici Diretti (DAG - Directed Acyclic Graphs)
- Apprendimento di reti bayesiane con algoritmo Hill-Climbing
- Modelli con e senza conoscenze a priori
- Implementazione di blacklist per definire:
  - Variabili target (Clri)
  - Variabili esogene (Cult)

### 4. Modelli Predittivi
- Regressione lineare con selezione delle variabili
- Confronto tra approcci:
  - Forward selection
  - Backward elimination
  - Stepwise (both directions)
- Criteri di selezione: AIC e BIC

## Requisiti

### Pacchetti R necessari
```r
install.packages("gRbase")
install.packages("gRain")
install.packages("gRim")
install.packages("glasso")
install.packages("igraph")
install.packages("bnlearn")
install.packages("ggm")
install.packages("pheatmap")

# Per il pacchetto graph (Bioconductor)
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("graph")
```

## Struttura del Progetto
```
FSM_Exam/
├── FSM_Exam.Rproj    # File di progetto RStudio
├── main.R            # Script principale con tutte le analisi
```

## Come Utilizzare

1. Aprire il progetto in RStudio tramite il file `FSM_Exam.Rproj`
2. Installare tutti i pacchetti necessari
3. Eseguire lo script `main.R` per riprodurre tutte le analisi

## Dataset

Il progetto utilizza il dataset `wine` del pacchetto `gRbase`, che contiene misurazioni chimiche di vini italiani classificati in tre cultivar diverse. Le variabili analizzate includono:
- **Clri**: Intensità del colore (variabile target)
- **Cult**: Tipo di cultivar (v1, v2, v3)
- Altre caratteristiche chimiche del vino

## Metodologia

L'analisi segue un approccio sistematico:
1. Esplorazione e visualizzazione dei dati
2. Stima di modelli grafici per comprendere le dipendenze condizionali
3. Costruzione di reti bayesiane per inferenza causale
4. Sviluppo di modelli predittivi per la variabile target

## Ulteriori Informazioni

Per una descrizione dettagliata delle metodologie, dei risultati ottenuti e delle interpretazioni statistiche, si rimanda al **report completo** del progetto.

---

