rm(list = ls())
require(tidyverse)
library(DiagrammeR)


mermaid('
graph TB;
D(Data with NUTS codes<br/>of unknown level and version) --> C
C[\"Classify version and level:<br><br><b>classify_nuts()</b>\"] --> DC(Data with NUTS codes<br>of e.g. level 2 in version 2006)
DC --> V[\"Convert between versions:<br><br><b>convert_nuts_version()</b>\"]
DC --> L[\"Convert between levels:<br><br><b>convert_nuts_level</b>()\"]
V --> DV(Data with NUTS codes<br>of e.g. level 2 in version 2021)
L --> DL(Data with NUTS codes<br>of e.g. level 1 in version 2006)

style C fill:#ffcc00
style V fill:#ffcc00
style L fill:#ffcc00
style D fill:#91a9ff
style DC fill:#91a9ff
style DV fill:#91a9ff
style DL fill:#91a9ff
', height = '70%', width = '70%')



mermaid("
graph TB;
D(Data with NUTS codes<br>of unknown level and version) --> C
C{\"Classify version and level:<br><br><b>classify_nuts()</b>\"} --> DC(Data with NUTS codes<br>of e.g. level 2 in version 2006)
DC --> V{\"Convert between versions:<br><br><b>convert_nuts_version()</b>\"}
DC --> L{\"Convert between levels:<br><br><b>convert_nuts_level</b>()\"}
V --> DV(Data with NUTS codes<br>of e.g. level 2 in version 2021)
L --> DL(Data with NUTS codes<br>of e.g. level 1 in version 2006)

style C fill:#79e8e4
style V fill:#79e8e4
style L fill:#79e8e4
style D fill:#edb8ff
style DC fill:#edb8ff
style DV fill:#edb8ff
style DL fill:#edb8ff
")

mermaid("
graph LR;
D(Data with NUTS codes<br>of unknown level and version) --> C
C{\"Classify version and level:<br><br><b>classify_nuts()</b>\"} --> DC(Data with NUTS codes<br>of e.g. level 2 in version 2006)
DC --> V{\"Convert between versions:<br><br><b>convert_nuts_version()</b>\"}
DC --> L{\"Convert between levels:<br><br><b>convert_nuts_level</b>()\"}
V --> DV(Data with NUTS codes<br>of e.g. level 2 in version 2021)
L --> DL(Data with NUTS codes<br>of e.g. level 1 in version 2006)

style C fill:#79e8e4
style V fill:#79e8e4
style L fill:#79e8e4
style D fill:#edb8ff
style DC fill:#edb8ff
style DV fill:#edb8ff
style DL fill:#edb8ff
")


