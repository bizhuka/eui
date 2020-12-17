*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

DEFINE set_default.
  IF &1 IS INITIAL.
   &1 = &2.
  ENDIF.
END-OF-DEFINITION.
