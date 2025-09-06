signature SSF =
  sig
    type sample
    type header
    type sound
    datatype format = UNSIGNED

    val writeSample: int * sample -> BinIO.outstream -> unit
    val writeHeader: header -> BinIO.outstream -> unit
    val writeSound:  sound  -> BinIO.outstream -> unit

    val readSample: int -> BinIO.instream -> sample
    val readHeader: BinIO.instream -> header
    val readSound:  int -> BinIO.instream -> sound

    val readFile: string -> header * sound list

    val sampleFromWord32: format -> Word32.word -> sample
  end

structure Ssf : SSF =
  struct
    type sample = Word32.word
    type header = { sampleRate     : int
                  , sampleFormat    : int
                  , bytesPerSample : int
                  , numOfSounds    : int
                  }
    type sound = int * sample list
    datatype format = UNSIGNED

    fun writeSample (sampleSize, inSample) outStream =
      let fun byteFromSample i =
            let val shiftAmount = Word.fromInt (8 * (sampleSize - i - 1))
            in  Word8.fromLarge (Word32.toLarge (Word32.>> (inSample, shiftAmount)))
            end
      in  BinIO.output (outStream, Word8Vector.tabulate (sampleSize, byteFromSample))
      end

    fun writeHeader (inHeader : header) outStream =
      let
      in  writeSample (4, 0wx53534646 : Word32.word)                 outStream;
          writeSample (4, Word32.fromInt (#sampleRate     inHeader)) outStream;
          writeSample (2, Word32.fromInt (#sampleFormat    inHeader)) outStream;
          writeSample (2, Word32.fromInt (#bytesPerSample inHeader)) outStream;
          writeSample (4, Word32.fromInt (#numOfSounds    inHeader)) outStream
      end

    fun writeSound inSound outStream =
      let val (bytesPerSample, sampleList) = inSound
      in  writeSample (4, Word32.fromInt (List.length sampleList)) outStream;
          List.foldr (fn (x, y) => writeSample (bytesPerSample, x) outStream) () sampleList
      end

    fun readSample sampleSize inStream =
      let val readVector = BinIO.inputN (inStream, sampleSize)
          fun shiftSumBytes (x : Word8.word , y : sample) =
            Word32.+ (Word32.fromLarge (Word8.toLarge x), Word32.<< (y, 0w8))
      in  Word8Vector.foldl shiftSumBytes (0w0 : sample) readVector
      end

    fun readHeader inStream =
      let val fileType = readSample 4 inStream
      in  if fileType = (0wx53534646 : sample)
          then
            { sampleRate     = Word32.toInt (readSample 4 inStream)
            , sampleFormat    = Word32.toInt (readSample 2 inStream)
            , bytesPerSample = Word32.toInt (readSample 2 inStream)
            , numOfSounds    = Word32.toInt (readSample 4 inStream)
            }
          else
            { sampleRate     = 0
            , sampleFormat    = 0
            , bytesPerSample = 0
            , numOfSounds    = 0
            }
      end

    fun readSound bytesPerSample inStream =
      let val numOfSamples = Word32.toInt (readSample 4 inStream)
      in  (bytesPerSample, List.tabulate (numOfSamples, (fn x => readSample bytesPerSample inStream)))
      end

    fun readFile fileName =
      let val inStream = BinIO.openIn fileName
          val fileHeader = readHeader inStream
          val readOneSound = fn x => readSound (#bytesPerSample fileHeader) inStream
          val fileSounds = List.tabulate (#numOfSounds fileHeader, readOneSound)
      in  BinIO.closeIn  inStream;
          (fileHeader, fileSounds)
      end

    fun sampleFromWord32 inFormat inWord =
      case inFormat
      of UNSIGNED => inWord

  end
