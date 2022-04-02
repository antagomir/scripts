Custom Jekyll Theme
=====================================

This site is being developed in the devel branch. Deploy to master
with ./deploy.sh - expect some minutes of delay sometimes

Re-build the site to see your changes. You can rebuild the site in many different ways, but the most common way is to run `jekyll serve`, which launches a web server and auto-regenerates your site when a file is update.d

Jekyll also offers powerful support for code snippets:

{% highlight ruby %}
def print_hi(name)
  puts "Hi, #{name}"
end
print_hi('Tom')
#=> prints 'Hi, Tom' to STDOUT.
{% endhighlight %}

Check out the [Jekyll docs][jekyll] for more info on how to get the most out of Jekyll. File all bugs/feature requests at [Jekyll’s GitHub repo][jekyll-gh]. If you have questions, you can ask them on [Jekyll’s dedicated Help repository][jekyll-help].

[jekyll]:      http://jekyllrb.com
[jekyll-gh]:   https://github.com/jekyll/jekyll
[jekyll-help]: https://github.com/jekyll/jekyll-help


This custom Jekyll theme is derived from Steve Miller's version of [Joel Glovier](http://joelglovier.com/)'s `jekyll-new` theme smashed with [Alex King](http://www.alexking.org)'s [Favepersonal](https://crowdfavorite.com/favepersonal/) theme for Wordpress. I used Favepersonal for my Wordpress site before abandoning it. You can see my site at [svmiller.github.io](http://svmiller.github.io).

Much of what is contained in here is derivative of those two works. That said, do observe the `embedpdf.html` and `image.html` files in the `_includes` directory. `embedpdf.html` uses Google Docs to allow for embedding of PDF files hosted on Dropbox. `image.html` provides fancier images than what is standard for Markdown. An example use of `embedpdf.html` can be observed in the `cv.md` file. An example use of `image.html` can be observed in the `about.md` file.

I use data-driven navigation, which you can see in the `menu.yml` file in the `_data` directory. There's also a `nav.html` file in the `_includes` directory with modified `header.html`.

`css` and `_sass` directories also functional, if a bit cluttered. Do observe new colors I created for `$clemson-orange` and `$clemson-purple` in `css/main.scss`.


