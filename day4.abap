FUNCTION ZNP_AOC_2024_DAY4.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EV_PART1) TYPE  I
*"     REFERENCE(EV_PART2) TYPE  I
*"----------------------------------------------------------------------
  DATA: lv_file  TYPE string,
        lv_day   TYPE c VALUE 4,
        lt_lines TYPE STANDARD TABLE OF string,
        lv_line  LIKE LINE OF lt_lines.

* Read input
  CONCATENATE gc_filepath lv_day gc_txt INTO lv_file.
  PERFORM read_input TABLES lt_lines
                     USING  lv_file.

  CHECK sy-subrc = 0.

* Part 1
  DATA: lv_nextline LIKE LINE OF lt_lines,
        lv_char     TYPE c,
        lv_index    TYPE i,
        lv_tabix    TYPE i,
        lv_refix    TYPE i.

  LOOP AT lt_lines INTO lv_line.
    lv_refix = sy-tabix.
    DO strlen( lv_line ) TIMES.
      lv_index = sy-index - 1.
      lv_char = lv_line+lv_index(1).
      IF lv_char = 'X'.
        IF lv_index < strlen( lv_line ) - 3. "check right
          lv_index = lv_index + 1.
          IF lv_line+lv_index(1) = 'M'.
            lv_index = lv_index + 1.
            IF lv_line+lv_index(1) = 'A'.
              lv_index = lv_index + 1.
              IF lv_line+lv_index(1) = 'S'.
                ADD 1 TO ev_part1.
              ENDIF.
            ENDIF.
          ENDIF.
          lv_index = sy-index - 1.
        ENDIF.
        IF lv_index >= 3. "check left
          lv_index = lv_index - 1.
          IF lv_line+lv_index(1) = 'M'.
            lv_index = lv_index - 1.
            IF lv_line+lv_index(1) = 'A'.
              lv_index = lv_index - 1.
              IF lv_line+lv_index(1) = 'S'.
                ADD 1 TO ev_part1.
              ENDIF.
            ENDIF.
          ENDIF.
          lv_index = sy-index - 1.
        ENDIF.
        IF lv_refix <= lines( lt_lines ) - 3. "check down
          lv_tabix = lv_refix + 1.
          READ TABLE lt_lines INDEX lv_tabix INTO lv_nextline.
          IF lv_nextline+lv_index(1) = 'M'.
            lv_tabix = lv_tabix + 1.
            READ TABLE lt_lines INDEX lv_tabix INTO lv_nextline.
            IF lv_nextline+lv_index(1) = 'A'.
              lv_tabix = lv_tabix + 1.
              READ TABLE lt_lines INDEX lv_tabix INTO lv_nextline.
              IF lv_nextline+lv_index(1) = 'S'.
                ADD 1 TO ev_part1.
              ENDIF.
            ENDIF.
          ENDIF.
          lv_index = sy-index - 1.
        ENDIF.
        IF lv_refix > 3. "check up
          lv_tabix = lv_refix - 1.
          READ TABLE lt_lines INDEX lv_tabix INTO lv_nextline.
          IF lv_nextline+lv_index(1) = 'M'.
            lv_tabix = lv_tabix - 1.
            READ TABLE lt_lines INDEX lv_tabix INTO lv_nextline.
            IF lv_nextline+lv_index(1) = 'A'.
              lv_tabix = lv_tabix - 1.
              READ TABLE lt_lines INDEX lv_tabix INTO lv_nextline.
              IF lv_nextline+lv_index(1) = 'S'.
                ADD 1 TO ev_part1.
              ENDIF.
            ENDIF.
          ENDIF.
          lv_index = sy-index - 1.
        ENDIF.
        IF lv_index < strlen( lv_line ) - 3 AND lv_refix <= lines( lt_lines ) - 3. "check diagonal - right/down
          lv_index = lv_index + 1.
          lv_tabix = lv_refix + 1.
          READ TABLE lt_lines INDEX lv_tabix INTO lv_nextline.
          IF lv_nextline+lv_index(1) = 'M'.
            lv_index = lv_index + 1.
            lv_tabix = lv_tabix + 1.
            READ TABLE lt_lines INDEX lv_tabix INTO lv_nextline.
            IF lv_nextline+lv_index(1) = 'A'.
              lv_index = lv_index + 1.
              lv_tabix = lv_tabix + 1.
              READ TABLE lt_lines INDEX lv_tabix INTO lv_nextline.
              IF lv_nextline+lv_index(1) = 'S'.
                ADD 1 TO ev_part1.
              ENDIF.
            ENDIF.
          ENDIF.
          lv_index = sy-index - 1.
        ENDIF.
        IF lv_index >= 3 AND lv_refix > 3. "check diagonal - left/up
          lv_index = lv_index - 1.
          lv_tabix = lv_refix - 1.
          READ TABLE lt_lines INDEX lv_tabix INTO lv_nextline.
          IF lv_nextline+lv_index(1) = 'M'.
            lv_index = lv_index - 1.
            lv_tabix = lv_tabix - 1.
            READ TABLE lt_lines INDEX lv_tabix INTO lv_nextline.
            IF lv_nextline+lv_index(1) = 'A'.
              lv_index = lv_index - 1.
              lv_tabix = lv_tabix - 1.
              READ TABLE lt_lines INDEX lv_tabix INTO lv_nextline.
              IF lv_nextline+lv_index(1) = 'S'.
                ADD 1 TO ev_part1.
              ENDIF.
            ENDIF.
          ENDIF.
          lv_index = sy-index - 1.
        ENDIF.
        IF lv_index < strlen( lv_line ) - 3 AND lv_refix > 3. "check diagonal - right/up
          lv_index = lv_index + 1.
          lv_tabix = lv_refix - 1.
          READ TABLE lt_lines INDEX lv_tabix INTO lv_nextline.
          IF lv_nextline+lv_index(1) = 'M'.
            lv_index = lv_index + 1.
            lv_tabix = lv_tabix - 1.
            READ TABLE lt_lines INDEX lv_tabix INTO lv_nextline.
            IF lv_nextline+lv_index(1) = 'A'.
              lv_index = lv_index + 1.
              lv_tabix = lv_tabix - 1.
              READ TABLE lt_lines INDEX lv_tabix INTO lv_nextline.
              IF lv_nextline+lv_index(1) = 'S'.
                ADD 1 TO ev_part1.
              ENDIF.
            ENDIF.
          ENDIF.
          lv_index = sy-index - 1.
        ENDIF.
        IF lv_index >= 3 AND lv_refix <= lines( lt_lines ) - 3. "check diagonal - left/down
          lv_index = lv_index - 1.
          lv_tabix = lv_refix + 1.
          READ TABLE lt_lines INDEX lv_tabix INTO lv_nextline.
          IF lv_nextline+lv_index(1) = 'M'.
            lv_index = lv_index - 1.
            lv_tabix = lv_tabix + 1.
            READ TABLE lt_lines INDEX lv_tabix INTO lv_nextline.
            IF lv_nextline+lv_index(1) = 'A'.
              lv_index = lv_index - 1.
              lv_tabix = lv_tabix + 1.
              READ TABLE lt_lines INDEX lv_tabix INTO lv_nextline.
              IF lv_nextline+lv_index(1) = 'S'.
                ADD 1 TO ev_part1.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDLOOP.

* Part 2
  DATA: lv_upper LIKE LINE OF lt_lines,
        lv_lower LIKE LINE OF lt_lines,
        lv_ixl   TYPE i,
        lv_ixr   TYPE i.

  LOOP AT lt_lines INTO lv_line.
    lv_refix = sy-tabix.
    DO strlen( lv_line ) TIMES.
      lv_index = sy-index - 1.
      lv_char = lv_line+lv_index(1).
      IF lv_char = 'A'.
        IF lv_index > 0 AND lv_index < strlen( lv_line ) - 1 AND lv_refix > 1 AND lv_refix < lines( lt_lines ).
          READ TABLE lt_lines INTO: lv_upper INDEX lv_refix - 1,
                                    lv_lower INDEX lv_refix + 1.
          lv_ixl = lv_index - 1.
          lv_ixr = lv_index + 1.
          IF ( ( lv_upper+lv_ixl(1) = 'M' AND lv_lower+lv_ixr(1) = 'S' ) OR ( lv_upper+lv_ixl(1) = 'S' AND lv_lower+lv_ixr(1) = 'M' ) ) AND
             ( ( lv_upper+lv_ixr(1) = 'M' AND lv_lower+lv_ixl(1) = 'S' ) OR ( lv_upper+lv_ixr(1) = 'S' AND lv_lower+lv_ixl(1) = 'M' ) ).
            ADD 1 TO ev_part2.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDLOOP.

ENDFUNCTION.