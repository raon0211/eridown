Eridown
=========

Eridown is a project that aims for a *scalable*, *fast*, and *accurate* Markdown parser written in the powerful Scala language. Also, it provides an extension of Markdown, named *Eridown*, which has many convenient syntaxes such as tables, and definition lists.

# Features

## Scalability

The main aim of this project is to support scalability in markdown parsers. Many contemporary markdown parsers cannot be *extended* or *downsized*, i.e. everyone had to use all the CommonMark specs, or GFM specs, etc. 

However Eridown changes this paradigm. By only writing a markdown *element object* and *generator*, you can easily extend the markdown syntax or exclude some syntaxes you don't want to include. This means that you could even create your own markdown spec!

## Speed

Another aim of this project is speed. By introducing the *scanner*, we could increase the speed of our parser. Detailed benchmarks will follow soon.

## Accuracy

Every parser must not abandon accuracy. Eridown will produce accurate results according to the CommonMark specifications. Were you to find any errors in Eridown, feel free to open an issue at our issue tracker.