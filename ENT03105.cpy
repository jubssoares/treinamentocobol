      *----------------------------------------------------------------*
      *      BOOK DO TREINAMENTO EM COBOL/MAINFRAME - EXERCÍCIO 3      *
      *----------------------------------------------------------------*
      * NOME DO BOOK: ENT03105                                         *
      * DESCRIÇÃO   : ARQUIVO DE DEPOSITOS EM CONTA CORRENTE           *
      * TAMANHO     : 56 BYTES                                         *
      * AUTORA      : JULIANA SOARES                                   *
      * DATA        : 19/01/2023                                       *
      *----------------------------------------------------------------*
      *                       DADOS DE ENTRADA                         *
      *----------------------------------------------------------------*
      * COD-AGENCIA: CÓDIGO DA AGENCIA                                 *
      * NUM-CONTA  : NUMERO DA CONTA                                   *
      * NOM-CLIENTE: NOME DO CLIENTE                                   *
      * DAT-EMPRE  : DATA DO EMPRESTIMO                                *
      *----------------------------------------------------------------* 

        01 ARQENT01-REGISTRO.                                             
          03 ARQENT01-COD-AGENCIA          PIC 9(03) VALUE 0.            
          03 ARQENT01-NUM-CONTA            PIC 9(03) VALUE 0.            
          03 ARQENT01-NOM-CLIENTE          PIC X(40) VALUE SPACES.       
          03 ARQENT01-DAT-EMPRE            PIC X(10) VALUE SPACES. 