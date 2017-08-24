package in.suhj.eridown.core

class Scanner private (
    val buffer: String,
    var position: Int = 0,
    var start: Int,
    var end: Int
) {
    import Scanner._

    def this(buffer: String, position: Int) {
        this(buffer, position, position, buffer.length)
    }

    private var marker: Int = position

    def text = buffer.substring(start, end)
    def ahead = buffer.substring(position)

    def currentChar: Char = charAt(0)
    def charAt(offset: Int): Char = {
        val index = position + offset
        if (index >= start && index < end) buffer(position + offset)
        else '\0'
    }

    def atStart = position == start
    def atEnd = position >= end
    def atLineEnd = isLineEnd(currentChar)
    def atWhitespace = isWhitespace(currentChar)

    def reads(char: Char) = char == currentChar
    def reads(str: String) = ahead.startsWith(str)
    def readsAny(chars: Seq[Char]) = chars.exists(reads _)

    def mark() = marker = position
    def extract =
        if (marker <= position) buffer.substring(marker, position)
        else buffer.substring(position, marker)
    def extractIdentifier = {
        def atValidStart = Character.isLetter(currentChar) || reads('_')
        def atValidLetter = atValidStart || Character.isDigit(currentChar)

        mark()
        if (atValidStart) {
            skip(1)
            while (position < end && atValidLetter) skip(1)
        }

        extract
    }

    def skip(offset: Int) = {
        position += offset

        if (position >= end) {
            position = end
        } else if (position < start) {
            position = start
        }
    }
    def skipWhitespace() = {
        while (isWhitespace(currentChar) & !atEnd) {
            skip(1)
        }
    }
    def skipLineEnd() = {
        while (isLineEnd(currentChar) & !atEnd) {
            skip(1)
        }
    }
    def skipToLineEnd() = {
        while (!isLineEnd(currentChar) & !atEnd) {
            skip(1)
        }
    }
    def skipToNextLine() = {
        skipToLineEnd()
        skipLineEnd()
    }

    def seek(char: Char): Int = seek(String.valueOf(char))
    def seek(str: String): Int = ahead.indexOf(str)
    def seekAny(chars: Seq[Char]): Int = {
        val indexes = chars.map(seek).filter(_ >= 0)
        if (indexes.isEmpty) -1
        else indexes.min
    }

    def find(char: Char): Boolean = find(String.valueOf(char))
    def find(str: String): Boolean = {
        val index = buffer.indexOf(str, position)

        if (index >= start && index < end) {
            position = index
            return true
        }

        return false
    }
}

object Scanner {
    def apply(buffer: String) = new Scanner(buffer, 0)
    def apply(buffer: String, start: Int) = new Scanner(buffer, start)
    def apply(buffer: String, start: Int, end: Int) = new Scanner(buffer, start, start, end)

    def isLineEnd(char: Char) = char == '\n' || char == '\r' || char == '\0'
    def isWhitespace(char: Char) = char == ' ' || char == '\t'
}
