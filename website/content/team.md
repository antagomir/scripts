---
title: "Team"
layout: gridlay
excerpt: "Team members"
sitemap: false
permalink: /team/
---

# Research team

**Welcome to contact us to discuss research/study positions and collaboration.** See also [open positions](../recruit/)  


<div class="col-sm-12 clearfix">

  <div class="col-sm-3 clearfix" width="25%"> 
  <img src="../images/teampic/leo_sci.jpg" class="img-responsive" width="100%" style="float: left" /><br/>

  <i><leo.lahti@iki.fi></i><br/>
  
  <a href="http://orcid.org/0000-0001-5537-637X"><img src='../images/orcid_qrcode_leolahti.png' title='Leo Lahti ORCID QR code 0000-0001-5537-637X' alt='ORCID: 0000-0001-5537-637X' width='33%'/></a> <a href="https://twitter.com/antagomir"><img src='../images/twitter.jpeg' title='antagomir@twitter' alt='https://twitter.com/antagomir' width='25%'/></a>
  
  </div>

  <h4>Leo Lahti (Group leader)</h4>

  <ul style="overflow: hidden">
  <li>Associate Professor (Data Science)</li>
  <li>Academy of Finland Research Fellow 2016-2021</li>
  <li>Director of Turku Center for Computational Humanities</li>  
  <li>Founding member of <a href="http://www.helsinki.fi/computational-history">Helsinki Computational History Group</a></li>    
  <li><a href="https://turkucitydata.fi">Turku City Data Co.</a> Board member and scientific advisor.</li>  
  <li><a href="https://blueprintgenetics.com">Blueprint Genetics</a>. Scientific Advisor (AI & ML)</li>
  <li><a href="http://fi.okfn.org/wg/openscience/">Open Science work group</a>, OKF Finland. Founding member.</li>

  <li><a href="../images/pgp_lahti_pm.asc">PGP key</a>;
  <a href="http://orcid.org/0000-0001-5537-637X">ORCID: 0000-0001-5537-637X</a>;
<a href="http://www.researcherid.com/rid/G-3170-2010">ResearcherID: G-3170-2010</a>;
<a href="https://tinyurl.com/ng6g6tk">Google Scholar</a>;
<a href="https://www.scopus.com/authid/detail.uri?authorId=8679063700">Scopus</a>;
<a href="http://www.researchgate.net/profile/Leo_Lahti/">ResearchGate</a>;
<a href="https://publons.com/author/246930/leo-lahti#stats">Publon</a>;
<a href="https://impactstory.org/u/0000-0001-5537-637X">ImpactStory</a>;
<a href="http://depsy.org/person/333684">Depsy software impact</a>; 
<a href="http://loop.frontiersin.org/people/295152/overview">Loop</a>; 
<a href="https://www.scienceopen.com/user/statistics/leo_lahti">ScienceOpen</a>;
<a href="http://www.citeulike.org/author/Lahti:L">CiteUlike</a>; 
<a href="http://www.ncbi.nlm.nih.gov/sites/myncbi/collections/public/1VaRtFbzqhfLWsXzDa1c5CSQK">PubMed</a>; 
<a href="https://tuhat.halvi.helsinki.fi/portal/en/persons/leo-mikael-lahti%285d23e9ba-1f39-42f0-b23b-77fe12413479%29.html">TUHAT</a></li>

</ul>

</div>

<p></p>

## Postdocs
{% assign number_printed = 0 %}
{% for member in site.data.postdocs %}

{% assign even_odd = number_printed | modulo: 2 %}

{% if even_odd == 0 %}
<div class="row">
{% endif %}

<div class="col-sm-6 clearfix">
  <img src="{{ site.url }}{{ site.baseurl }}/images/teampic/{{ member.photo }}" class="img-responsive" width="25%" style="float: left" />
  <h4>{{ member.name }}</h4>
  <i>{{ member.info }}<br>email: <{{ member.email }}></i>
  <ul style="overflow: hidden">
  
  {% if member.number_educ == 1 %}
  <li> {{ member.education1 }} </li>
  {% endif %}
  
  {% if member.number_educ == 2 %}
  <li> {{ member.education1 }} </li>
  <li> {{ member.education2 }} </li>
  {% endif %}
  
  {% if member.number_educ == 3 %}
  <li> {{ member.education1 }} </li>
  <li> {{ member.education2 }} </li>
  <li> {{ member.education3 }} </li>
  {% endif %}
  
  {% if member.number_educ == 4 %}
  <li> {{ member.education1 }} </li>
  <li> {{ member.education2 }} </li>
  <li> {{ member.education3 }} </li>
  <li> {{ member.education4 }} </li>
  {% endif %}
  
  </ul>
</div>

{% assign number_printed = number_printed | plus: 1 %}

{% if even_odd == 1 %}
</div>
{% endif %}

{% endfor %}

{% assign even_odd = number_printed | modulo: 2 %}
{% if even_odd == 1 %}
</div>
{% endif %}

<p></p>


## PhD Students 
{% assign number_printed = 0 %}
{% for member in site.data.phds %}

{% assign even_odd = number_printed | modulo: 2 %}

{% if even_odd == 0 %}
<div class="row">
{% endif %}

<div class="col-sm-6 clearfix">
  <img src="{{ site.url }}{{ site.baseurl }}/images/teampic/{{ member.photo }}" class="img-responsive" width="25%" style="float: left" />
  <h4>{{ member.name }}</h4>
  <i>{{ member.info }}<br>email: <{{ member.email }}></i>
  <ul style="overflow: hidden">
  
  {% if member.number_educ == 1 %}
  <li> {{ member.education1 }} </li>
  {% endif %}
  
  {% if member.number_educ == 2 %}
  <li> {{ member.education1 }} </li>
  <li> {{ member.education2 }} </li>
  {% endif %}
  
  {% if member.number_educ == 3 %}
  <li> {{ member.education1 }} </li>
  <li> {{ member.education2 }} </li>
  <li> {{ member.education3 }} </li>
  {% endif %}
  
  {% if member.number_educ == 4 %}
  <li> {{ member.education1 }} </li>
  <li> {{ member.education2 }} </li>
  <li> {{ member.education3 }} </li>
  <li> {{ member.education4 }} </li>
  {% endif %}
  
  </ul>
</div>

{% assign number_printed = number_printed | plus: 1 %}

{% if even_odd == 1 %}
</div>
{% endif %}

{% endfor %}

{% assign even_odd = number_printed | modulo: 2 %}
{% if even_odd == 1 %}
</div>
{% endif %}



<!--
## Visitors

{% assign number_printed = 0 %}
{% for member in site.data.visitors %}

{% assign even_odd = number_printed | modulo: 2 %}

{% if even_odd == 0 %}
<div class="row">
{% endif %}

<div class="col-sm-6 clearfix">
  <img src="{{ site.url }}{{ site.baseurl }}/images/teampic/{{ member.photo }}" class="img-responsive" width="25%" style="float: left" />
  <h4>{{ member.name }}</h4>
  <i>{{ member.info }}<br>email: <{{ member.email }}></i>  
  <ul style="overflow: hidden">
  
  {% if member.number_educ == 1 %}
  <li> {{ member.education1 }} </li>
  {% endif %}
  
  {% if member.number_educ == 2 %}
  <li> {{ member.education1 }} </li>
  <li> {{ member.education2 }} </li>
  {% endif %}
  
  {% if member.number_educ == 3 %}
  <li> {{ member.education1 }} </li>
  <li> {{ member.education2 }} </li>
  <li> {{ member.education3 }} </li>
  {% endif %}
  
  {% if member.number_educ == 4 %}
  <li> {{ member.education1 }} </li>
  <li> {{ member.education2 }} </li>
  <li> {{ member.education3 }} </li>
  <li> {{ member.education4 }} </li>
  {% endif %}
  
  </ul>
</div>


{% assign number_printed = number_printed | plus: 1 %}

{% if even_odd == 1 %}
</div>
{% endif %}

{% endfor %}

{% assign even_odd = number_printed | modulo: 2 %}
{% if even_odd == 1 %}
</div>
{% endif %}

-->

## Research assistants

{% assign number_printed = 0 %}
{% for member in site.data.students %}

{% assign even_odd = number_printed | modulo: 2 %}

{% if even_odd == 0 %}
<div class="row">
{% endif %}

<div class="col-sm-6 clearfix">
  <h4>{{ member.name }}</h4>
  <i>{{ member.info }}</i>
  <ul style="overflow: hidden">
  
  {% if member.number_educ == 1 %}
  <li> {{ member.education1 }} </li>
  {% endif %}
  
  {% if member.number_educ == 2 %}
  <li> {{ member.education1 }} </li>
  <li> {{ member.education2 }} </li>
  {% endif %}
  
  {% if member.number_educ == 3 %}
  <li> {{ member.education1 }} </li>
  <li> {{ member.education2 }} </li>
  <li> {{ member.education3 }} </li>
  {% endif %}
  
  {% if member.number_educ == 4 %}
  <li> {{ member.education1 }} </li>
  <li> {{ member.education2 }} </li>
  <li> {{ member.education3 }} </li>
  <li> {{ member.education4 }} </li>
  {% endif %}
  
  </ul>
</div>

{% assign number_printed = number_printed | plus: 1 %}

{% if even_odd == 1 %}
</div>
{% endif %}

{% endfor %}

{% assign even_odd = number_printed | modulo: 2 %}
{% if even_odd == 1 %}
</div>
{% endif %}


## Alumni

- Prashant Gaikwad
- Aaro Salosensaari
- Anastasia Karavaeva


<!--
## Former students
**PhD Students** Jose Caldas (With Prof Samuel Kaski; Aalto 2012)
-->

<!--**Master Students** Anastasia Karavaeva, Binu Matthew, Leila Paquay (Leuven 2018); Hege Roivainen (Helsinki 2017); Marnix Denys (KU Leuven 2017); Tineka Blake (Wageningen 2015); Emilio Ugaldes Morales (Wageningen 2014)-->

<!--**Bachelor Students** Maija Nevala (TKK 2008); Jyry Suvilehto (TKK 2007)-->
 
   

<!--
## Administrative Support
<a href="mailto:support@utu.fi">N.N</a> is helping us. and other groups) with administration.
-->



