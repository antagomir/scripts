---
title: "Publications"
layout: gridlay
excerpt: "Publications."
sitemap: false
permalink: /publications/
---


Publications
============

The complete bibliography is available in [BibTex](https://github.com/openresearchlabs/openresearchlabs.github.io/blob/build/publications/bibtex/lahti.bib). You can find most papers and other outputs from [here](https://github.com/openresearchlabs/openresearchlabs.github.io/tree/master/publications), or just ask by email.

For code, data, and audiovisual material see the separate [code](../code/) and [media](../media/) pages.


<!-- This is for altmetrics padges from http://www.altmetric.com/-->

<script type='text/javascript' src='https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js'></script>


## Selected papers

{% bibliography --file bibtex/lahti --query @*[keywords=highlight] %}


## Computational biosciences

{% bibliography --file bibtex/lahti --query @*[status=bioscience] %}
{% bibliography --file bibtex/lahti --query @*[status=datascience] %}


## Digital humanities and computational social science

{% bibliography --file bibtex/lahti  --query @*[status=dh] %}


## Open science

{% bibliography --file bibtex/lahti  --query @*[status=openscience] %}


## Popularized articles and general writings

{% bibliography --file bibtex/lahti  --query @*[status=general] %}


## Theses and assignments

For source material (LaTeX code, figures etc.) of these theses, see [Github](https://github.com/antagomir/thesis).

{% bibliography --file bibtex/lahti  --query @*[category=thesis] %}



<!--The material is presented to ensure timely dissemination of scholarly and technical work. While I aim to grant CC or other open source/copyleft licenses for the content wherever possible, kindly note that copyright in the external links and all rights therein are retained by authors or by other copyright holders.-->

