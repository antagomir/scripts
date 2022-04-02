---
title: "Publishing data and code"
layout: gridlay
excerpt: "Release"
sitemap: false
permalink: /release/
---


# How to publish and archive research software or source code

 * **Choose open license** The code is not really "open" without an open license. Common ones include MIT, FreeBSD and GPL3. The MIT license is often recommended due to its simplicity. See [Quick guide to software licensing for a scientist programmer](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1002598). See also the brief [memo on open licensing of scientific material](../licensing/).

 * **Add version number**

 * **Document the experiments**. At the minimum, list where the data is located (preferably link directly in the code) and which scripts should be run to replicate the experiments. Sometimes people use electronic notebooks (Rmd / Jupyter) to share analyses. This can combine the code with automatically generated figures etc and directly show the outputs of the code. Add a README.

 * **Long-term preservation** Github is not guaranteed to continue and it is maintained by a private party. It is recommended to make a git "release" and store that in Zenodo with a DOI. Then the DOI provides permanent access to the exact code that was used in the publication, and that zip file will remain available in the academic Zenodo service. If you want you can also create a DOI for the repository. Good to notice that the repository and exact version at a given time may be different things. You can check [this](https://guides.github.com/activities/citable-code/) and [this2](https://genr.eu/wp/cite/) for details.


## Tips on data science project git repo organization

* [PLoS Comput Biol. 2016 Jul; 12(7): **Ten Simple Rules for Taking Advantage of Git and GitHub**. https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4945047/](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4945047/) 
* [Human in a Machine World. May 25, 2016: **Folder Structure for Data Analysis**. https://medium.com/human-in-a-machine-world/folder-structure-for-data-analysis-62a84949a6ce](https://medium.com/human-in-a-machine-world/folder-structure-for-data-analysis-62a84949a6ce)
* [**Cookiecutter Data Science** - A logical, reasonably standardized, but flexible project structure for doing and sharing data science work. https://drivendata.github.io/cookiecutter-data-science/](https://drivendata.github.io/cookiecutter-data-science/)
* [Thinking on Data. December 9, 2018: **Best practices organizing data science projects**. https://www.thinkingondata.com/how-to-organize-data-science-projects/](https://www.thinkingondata.com/how-to-organize-data-science-projects/)