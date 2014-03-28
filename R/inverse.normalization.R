btw jos joskus tarvii tehdä inverse 
                 normalizationia R:llä, ni tässäpä koodinpätkä.
18:23 < juuussi> y <- qnorm((rank(x,na.last="keep")-0.5)/sum(!is.na(x))
18:27 < juuussi> ennoo kyllä varma miks tossa on toi -0.5
19:18 < juuussi> tosin nyt keksin. koska tossa siis otetaan noi rankit, sit 
                 jaetaan se rankkien lukumäärällä, ja siitä otetaan sitten tuo 
                 qnorm..
19:19 < juuussi> ni jos vektorissa x on vaan yksi luku, niin sen rankiksi tulee 
                 1, ja se jaetaan 1:lle, jollon sitku koitetaan antaa qnormille 
                 todennäköisyydeksi 1, niin siitä palautuu Inf
