
el comentario es "java -cp .:TestJ2.jar TestJ2"

enchufar Chamuyo
enchufar MostrarNumerito()

gringo Jvm
       "invokestatic QRT m_void_void"
       mVoidVoid de () en ()

gringo Jvm
       "invokestatic QRT m_int_int"
       mIntInt de Numerito en Numerito

gringo Jvm
       "invokestatic QRT m_int_int_void"
       mIntIntVoid de Numerito en Numerito en ()

gringo Jvm
       "invokestatic QRT m_char_void"
       mCharVoid de Letra en ()

gringo Jvm
       "invokestatic QRT m_void_char"
       mVoidChar de () en Letra

gringo Jvm
       "invokestatic QRT m_string_void"
       mStringVoid de Texto en ()

gringo Jvm
       "invokestatic QRT m_void_string"
       mVoidString de () en Texto

gringo Jvm
       "invokestatic QRT m_void_qrt"
       mVoidQrt de () en Pendorcho "QRT"

gringo Jvm
       "invokestatic QRT m_qrt_void"
       mQrtVoid de Pendorcho "QRT" en ()

gringo Jvm
       "invokestatic QRT m_boolean_void"
       mBooleanVoid de Posta en ()

gringo Jvm
       "invokestatic QRT m_void_boolean_true"
       mVoidBooleanTrue de () en Posta

gringo Jvm
       "invokestatic QRT m_void_boolean_false"
       mVoidBooleanFalse de () en Posta

gringo Jvm "getstatic java/lang/System out"
       mOut de Pendorcho "java/io/PrintStream"

gringo Jvm "println"
       mPrintln
       de Pendorcho "java/io/PrintStream" en Texto en ()

gringo Jvm "new" big
       de Texto en Pendorcho "java/math/BigDecimal"
gringo Jvm "divide" bigDiv
       de Pendorcho "java/math/BigDecimal"
       en Pendorcho "java/math/BigDecimal"
       en Pendorcho "java/math/BigDecimal"
gringo Jvm "toString" unbig
       de Pendorcho "java/math/BigDecimal" en Texto

el programa es
  mVoidVoid ();
  mIntIntVoid (mIntInt 10#) (mIntInt 20#);
  mCharVoid 'ñ';
  mCharVoid (mVoidChar ());
  mStringVoid "Hola múndo!";
  mStringVoid (mVoidString ());
  mQrtVoid (mVoidQrt ());
  mPrintln mOut "Desde adentro";
  escupir (unbig
            (bigDiv
              (big "10000.0")
              (big "200000"))); escupir "\n";
  escupir (mostrar (mVoidBooleanFalse ())); escupir "\n";
  escupir (mostrar (mVoidBooleanTrue ())); escupir "\n";
  mBooleanVoid No;
  mBooleanVoid Sí

