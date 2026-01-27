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
 * PDF files associated with such sources and works (including the multiple works an edited volume might contain, and the ability to jump to the PDF page of a particular paper)
 * Personal notes associated with any of the above
 * Notes taken during talks, meetings, seminars
 * Any other files or folders associated with any of the above (including notes)
 * Ability to manage (rename, move, etc.) files and folders while not losing associations with any of the above information
 * Associations between works and entries in your bibliography manager (currently integrates with Zotero and Mendeley)

Hypernomicon keeps track of all of the above in a highly structured, thoroughly indexed and user-friendly relational database, and automatically generates semantic hyperlinks between all of them so that you are constantly informed of connections in the material you hadn't realized.

## Getting started ##

The best way to get started with Hypernomicon is to download and install the latest
[Hypernomicon release](https://sourceforge.net/projects/hypernomicon/files/latest/download).

Or you can clone this repository and build from source (see below).

## Learning ##

To learn how to use Hypernomicon, watch and follow along with the [tutorial videos](https://www.youtube.com/playlist?list=PLCDXooVJfr1JKHT83awarYoIOhp0Xqr-B).

Also check out the [FAQ document](https://sourceforge.net/p/hypernomicon/wiki/FAQ/) and [SourceForge discussion forums](https://sourceforge.net/p/hypernomicon/discussion/).

## Issues and Contributions ##

If you have a non-programming-related question or topic to discuss, please post to the [SourceForge discussion forums](https://sourceforge.net/p/hypernomicon/discussion/).

To report a bug or make a feature request as an end user, please [create a ticket at SourceForge](https://sourceforge.net/p/hypernomicon/tickets/).

If you are a programmer and have a code-related issue, bug, question, or feature request, please post to the [GitHub issue tracker](https://github.com/jasonwinning/hypernomicon/issues/).

Contributions can be submitted via [Pull requests](https://github.com/jasonwinning/hypernomicon/pulls/).

## Building Hypernomicon ##

### Prerequisites

* [JDK 25](https://adoptium.net/temurin/releases/?version=25) or later for building the 'master' branch
* A recent version of [Git](https://git-scm.com/downloads)
* [Maven](https://maven.apache.org/download.cgi) version 3.6.3 or greater

### How to build Hypernomicon ###

Use Git to get a copy of the source code onto your computer.

`$ git clone git@github.com:jasonwinning/hypernomicon.git`

On the project's root, run:

`mvn clean package`

It will create an executable jar under `target/hypernomicon-$version.jar`.

If you have trouble downloading the JxBrowser jar files to your local Maven repository, you can download them [from here](https://sourceforge.net/projects/hypernomicon/files/misc/jxbrowser-6.24.3.zip/download).

## Change Log/Release Notes

Can be found [here](https://sourceforge.net/p/hypernomicon/wiki/ReleaseNotes/).


## Authors

* **Jason Winning** - *Original design and development* - [website](https://jasonwinning.com)

See also the list of [contributors](https://github.com/jasonwinning/hypernomicon/contributors) who participated in this project.

## License

This project is licensed under the Apache 2.0 License + Commons Clause 1.0 - see the [LICENSE](https://jasonwinning.github.io/hypernomicon/LICENSE.html) file for details.

Hypernomicon is a non-commercial product and will always be free to use.

Hypernomicon uses [JxBrowser](https://www.teamdev.com/jxbrowser). You may not use JxBrowser separately from Hypernomicon without a separate license from TeamDev Ltd. Use of JxBrowser as part of Hypernomicon in any commercial software requires a commercial license from TeamDev Ltd.

## Acknowledgements

Thanks to [Danny Weltman](https://dannyweltman.com/) for design ideas and testing.

* Icons:

  * [FatCow Farm-Fresh Web Icons](http://www.fatcow.com/free-icons) [(Archived copy)](https://web.archive.org/web/20160323032439/http://www.fatcow.com/free-icons)
  * [Fugue Icons](http://p.yusukekamiyamane.com/)

* Additional third-party libraries used:
  * Apache [Commons Lang](https://commons.apache.org/proper/commons-lang/), [Commons Text](https://commons.apache.org/proper/commons-text/), [Commons IO](https://commons.apache.org/proper/commons-io/), [PDFBox](https://pdfbox.apache.org/), [Tika](https://tika.apache.org/), [Log4j](https://logging.apache.org/log4j/2.x/index.html)
  * [Guava](https://github.com/google/guava)
  * [PDF.js](https://mozilla.github.io/pdf.js/)
  * [jsoup](https://jsoup.org/)
  * [ICU4J](https://icu.unicode.org/home)
  * [ControlsFX](https://controlsfx.github.io/)
  * [JSON.simple](https://code.google.com/archive/p/json-simple/)
  * [ScribeJava](https://github.com/scribejava/scribejava)
  * [XMP Toolkit for Java](https://www.adobe.com/devnet/xmp.html)
  * [JODConverter](https://github.com/jodconverter/jodconverter)
  * [NetBeans Keyring](https://netbeans.apache.org/front/main/index.html)
  * [JUnit Jupiter](https://junit.org/junit5/)
  * [SLF4J Simple](http://www.slf4j.org)
  * [JBibTex](https://github.com/jbibtex/jbibtex)
  * [mark.js](https://markjs.io/)
