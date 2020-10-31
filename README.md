# Hypernomicon

[![Tutorial video](https://img.youtube.com/vi/JOTkAzh0qZE/0.jpg)](https://www.youtube.com/watch?v=JOTkAzh0qZE)

Hypernomicon is a personal database application for keeping track of information like theories, debates, arguments, concepts, etc. that might be used by people such as philosophers or others doing academic research.

Hypernomicon is perfect for: Anyone who works in a field (professionals, hobbyists, and students alike) where they have to keep track of several (or more) of the following:
 * Terminology
 * Concepts
 * Theoretical questions/Debates (hierarchically organized)
 * Theories (hierarchically organized)
 * Positions (hierarchically organized)
 * Arguments for theories/positions (and counterarguments)
 * Authors (and information about them such as website, affiliation, etc.)
 * The works authors have authored
 * Sources and authors associated with arguments, theories, positions, and debates
 * PDF files associated with such sources and works (including the multiple works an edited volume might contain, and ability to jump to the PDF page of a particular paper)
 * Personal notes associated with any of the above
 * Notes taken during talks, meetings, seminars
 * Any other files or folders associated with any of the above (including notes)
 * Ability to manage (rename, move, etc.) files and folders while not losing associations with any of the above information
 * Associations between works and entries in your bibliography manager (currently integrates with Zotero and Mendeley)

Hypernomicon keeps track of all of the above in a highly structured, thoroughly indexed and user friendly relational database, and automatically generates semantic hyperlinks between all of them so that you are constantly informed of ways all of your information is related that you had not realized.

## Getting started ##

The best way to get started with Hypernomicon is by downloading and installing on your developer machine the latest
[Hypernomicon release](https://sourceforge.net/projects/hypernomicon/files/latest/download).

Or you can clone this repository and build from source (see below).

## Learning ##

To learn how to use Hypernomicon, watch and follow along with the [tutorial videos](https://www.youtube.com/playlist?list=PLCDXooVJfr1JKHT83awarYoIOhp0Xqr-B).

Also check out the [FAQ document](https://sourceforge.net/p/hypernomicon/wiki/FAQ/) and [SourceForge discussion forums](https://sourceforge.net/p/hypernomicon/discussion/).

## Issues and Contributions ##

If you have a non-programming-related question or topic to discuss, please post to the [SourceForge discussion forums](https://sourceforge.net/p/hypernomicon/discussion/).

To report a bug or make a feature request from the point of view of a user, please [create a ticket at SourceForge](https://sourceforge.net/p/hypernomicon/tickets/).

If you are a programmer and have a code-related issue, bug, question, or feature request, please post to the [Github issue tracker](https://github.com/jasonwinning/hypernomicon/issues/).

Contributions can be submitted via [Pull requests](https://github.com/jasonwinning/hypernomicon/pulls/).

## Building Hypernomicon ##

### Prerequisites

* [JDK 11](https://adoptopenjdk.net/index.html?variant=openjdk11&ga=ga) or later for building 'master' branch
* A recent version of [Git](https://git-scm.com/downloads)
* [Maven](https://maven.apache.org/download.cgi) version 3.1.0 or greater

### How to build Hypernomicon ###

Use Git to get a copy of the source code onto your computer.

`$ git clone git@github.com:jasonwinning/hypernomicon.git`

On the project's root, run:

`mvn clean package`

It will create an executable jar under `target/hypernomicon-$version.jar`.


## Change Log/Release Notes

Can be found [here](https://sourceforge.net/p/hypernomicon/wiki/ReleaseNotes/).


## Authors

* **Jason Winning** - *Original design and development* - [website](http://jasonwinning.org)

See also the list of [contributors](https://github.com/jasonwinning/hypernomicon/contributors) who participated in this project.

## License

This project is licensed under the Apache 2.0 License + Commons Clause 1.0 - see the [LICENSE](http://htmlpreview.github.com/?https://github.com/jasonwinning/hypernomicon/blob/master/LICENSE.html) file for details.

Hypernomicon is a non-commercial product and will always be free to use.

Hypernomicon uses [JxBrowser](https://www.teamdev.com/jxbrowser). You may not use JxBrowser separately from Hypernomicon without a separate license from TeamDev Ltd. Use of JxBrowser as part of Hypernomicon in any commercial software requires a commercial license from TeamDev Ltd.

## Acknowledgements

* Icons:

  * [FatCow](http://www.fatcow.com/free-icons)
  * [Fugue](http://p.yusukekamiyamane.com/)

* Additional third-party libraries used:
  * Apache [Commons](https://commons.apache.org/), [PDFBox](https://pdfbox.apache.org/), [Tika](https://tika.apache.org/), [HttpClient](https://hc.apache.org/httpcomponents-client-ga/)
  * [Guava](https://github.com/google/guava)
  * [PDF.js](https://mozilla.github.io/pdf.js/)
  * [jsoup](https://jsoup.org/)
  * [jQuery](https://jquery.com/)
  * [ICU4J](http://site.icu-project.org/home)
  * [ControlsFX](http://fxexperience.com/controlsfx/)
  * [JSON.simple](https://code.google.com/archive/p/json-simple/)
  * [ScribeJava](https://github.com/scribejava/scribejava)
  * [XMP Toolkit for Java](https://www.adobe.com/devnet/xmp.html)
  * [Mammoth .docx to HTML converter](https://github.com/mwilliamson/java-mammoth)
  * [JBibTex](https://github.com/jbibtex/jbibtex)
  * [highlight](http://johannburkard.de/blog/programming/javascript/highlight-javascript-text-higlighting-jquery-plugin.html)
