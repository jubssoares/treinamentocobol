      *----------------------------------------------------------------*
      *      BOOK DO TREINAMENTO EM COBOL/MAINFRAME - EXERCÍCIO 4      *
      *----------------------------------------------------------------*
      * TAMANHO DO REGISTRO: 117 BYTES                                  *
      *----------------------------------------------------------------*
      * AUTOR: JULIANA SILVA SOARES                                    *
      * DATA: 25/01/2023                                               *
      *----------------------------------------------------------------*
      *                       DADOS DE SAIDA                           *
      *----------------------------------------------------------------*

       01 ARQSAI01-REGISTRO.
           03 ARQSAI01-COD-CLI                 PIC 9.999.999.999.
           03 FILLER                           PIC X(01) VALUE ';'.
           03 ARQSAI01-NOM-CLI                 PIC X(70) VALUE SPACES.
           03 FILLER                           PIC X(01) VALUE ';'.
           03 ARQSAI01-DAT-ATULZ                PIC X(26) VALUE SPACES.
           03 FILLER                           PIC X(01) VALUE ';'.
           03 ARQSAI01-QDT-ENDER               PIC Z.ZZ9.