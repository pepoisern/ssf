signature SSF =
  sig
    datatype format = UNSIGNED | INVALID

    type sample
    type header
    type sound

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
    datatype format = UNSIGNED | INVALID

    type sample = Word32.word
    type header = { sampleRate     : int
                  , sampleFormat   : format
                  , bytesPerSample : int
                  , numOfSounds    : int
                  }
    type sound = int * sample list

    fun writeSample (sampleSize, sample) outStream =
      let fun byteFromSample i =
            let val shiftAmount = Word.fromInt (8 * (sampleSize - i - 1))
            in  Word8.fromLarge (Word32.toLarge (Word32.>> (sample, shiftAmount)))
            end
      in  BinIO.output (outStream, Word8Vector.tabulate (sampleSize, byteFromSample))
      end

    fun intFromFormat format =
      case format
        of UNSIGNED => 0
         | _ => 0xFF

    fun formatFromInt n =
      case n
        of 0 => UNSIGNED
         | _ => INVALID

    fun writeHeader (header : header) outStream =
      let
      in  writeSample (4, 0wx53534646 : Word32.word)                 outStream;
          writeSample (4, Word32.fromInt (#sampleRate     header)) outStream;
          writeSample (2, Word32.fromInt (intFromFormat (#sampleFormat   header))) outStream;
          writeSample (2, Word32.fromInt (#bytesPerSample header)) outStream;
          writeSample (4, Word32.fromInt (#numOfSounds    header)) outStream
      end

    fun writeSound sound stream =
      let val (bytesPerSample, sampleList) = sound
      in  writeSample (4, Word32.fromInt (List.length sampleList)) stream;
          List.foldl (fn (x, y) => writeSample (bytesPerSample, x) stream) () sampleList
      end

    fun readSample sampleSize stream =
      let val readVector = BinIO.inputN (stream, sampleSize)
          fun shiftSumBytes (x : Word8.word , y : sample) =
            Word32.+ (Word32.fromLarge (Word8.toLarge x), Word32.<< (y, 0w8))
      in  Word8Vector.foldl shiftSumBytes (0w0 : sample) readVector
      end

    fun readHeader stream =
      let val fileType = readSample 4 stream
      in  if fileType = (0wx53534646 : sample)
          then
            { sampleRate     = Word32.toInt (readSample 4 stream)
            , sampleFormat   = formatFromInt (Word32.toInt (readSample 2 stream))
            , bytesPerSample = Word32.toInt (readSample 2 stream)
            , numOfSounds    = Word32.toInt (readSample 4 stream)
            }
          else
            { sampleRate     = 0
            , sampleFormat   = UNSIGNED
            , bytesPerSample = 0
            , numOfSounds    = 0
            }
      end

    fun readSound bytesPerSample stream =
      let val numOfSamples = Word32.toInt (readSample 4 stream)
      in  (bytesPerSample, List.tabulate (numOfSamples, (fn x => readSample bytesPerSample stream)))
      end

    fun readFile fileName =
      let val inStream = BinIO.openIn fileName
          val fileHeader = readHeader inStream
          val readOneSound = fn x => readSound (#bytesPerSample fileHeader) inStream
          val fileSounds = List.tabulate (#numOfSounds fileHeader, readOneSound)
      in  BinIO.closeIn  inStream;
          (fileHeader, fileSounds)
      end

    fun sampleFromWord32 format inWord =
      case format
        of UNSIGNED => inWord
         | _ => 0wx0 : sample

  end
