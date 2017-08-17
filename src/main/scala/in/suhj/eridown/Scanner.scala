package in.suhj.eridown

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

    var marker: Int = position

    def text = buffer.substring(start, end)
    def ahead = buffer.substring(position)

    def currentChar: Char =
        if (position >= start && position < end) buffer(position)
        else '\0'
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
    def reads(str: String) = {
        val endIndex = position + str.length

        if (endIndex >= end) false
        else str == buffer.substring(position, endIndex)
    }
    def readsAny(chars: Seq[Char]) = chars.exists(reads _)

    def mark() = marker = position
    def extract =
        if (marker <= position) buffer.substring(marker, position)
        else buffer.substring(position, marker)

    def skip(offset: Int) = position += offset
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
    def seek(str: String): Int =  {
        val index = buffer.indexOf(str, position)
        if (index >= 0) index - position
        else -1
    }
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
    def apply(scanner: Scanner) = new Scanner(scanner.buffer, scanner.position, scanner.start, scanner.end)
    def apply(scanner: Scanner, start: Int) = new Scanner(scanner.buffer, scanner.start, scanner.start, scanner.end)

    def isLineEnd(char: Char) = char == '\n' || char == '\r' || char == '\0'
    def isWhitespace(char: Char) = char == ' ' || char == '\t'
}
