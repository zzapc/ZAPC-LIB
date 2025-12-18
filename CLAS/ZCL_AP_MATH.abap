class ZCL_AP_MATH definition
  public
  final
  create public .

public section.

  class-methods DESVIACION_STD
    importing
      !I_DATOS type UPF_T_FLOAT
    returning
      value(DESVIACION) type F .
  class-methods REDONDEO_CONDICIONAL
    importing
      !CANTIDAD type ANY
      !PORCENTAJE type ANY optional
      !DIFERENCIA type ANY optional
    returning
      value(SALIDA) type MENGV13 .
  class-methods MEDIANA
    importing
      !I_DATOS type UPF_T_FLOAT
    returning
      value(MEDIANA) type F .
protected section.
private section.
endclass. "ZCL_AP_MATH definition
class ZCL_AP_MATH implementation.
METHOD desviacion_std.
  DATA: l_media     TYPE f,
        l_suma      TYPE f,
        l_elementos TYPE i,
        l_variacion TYPE f,
        l_valor     TYPE f.

  CLEAR: l_suma, l_media, l_elementos.
  desviacion = 0.

  LOOP AT i_datos INTO l_valor.
    ADD 1 TO l_elementos.
    ADD l_valor TO l_suma.
  ENDLOOP.

  CHECK l_elementos > 1.

  l_media = l_suma / l_elementos.

  CLEAR l_suma.
  LOOP AT i_datos INTO l_valor.
    l_suma = l_suma + ( ( l_valor - l_media ) ** 2 ).
  ENDLOOP.

*  l_variacion = ( 1 / ( l_elementos - 1 ) ) * l_suma.
  CHECK l_media NE 0.

  l_variacion = l_suma / l_elementos.

  desviacion = sqrt( l_variacion ).


ENDMETHOD.
METHOD mediana.
  DATA: datos TYPE upf_t_float.

  CLEAR mediana.

  IF i_datos IS INITIAL.
    RETURN.
  ENDIF.

  DATA(l_elementos) = lines( i_datos ).
  IF l_elementos = 1.
    mediana = i_datos[ 1 ].
    RETURN.
  ELSEIF l_elementos = 2.
    mediana = ( i_datos[ 1 ] + i_datos[ 2 ] ) / 2.
    RETURN.
  ENDIF.

  datos = i_datos.
  SORT datos.

  IF l_elementos MOD 2 = 1. "Impar
    l_elementos = l_elementos DIV 2 + 1.
    mediana = datos[ l_elementos ].
  ELSE.
    l_elementos = l_elementos DIV 2.
    DATA(l_siguiente) = l_elementos + 1.
    mediana = ( datos[ l_elementos ] + datos[ l_siguiente ] ) / 2.
  ENDIF.

ENDMETHOD.
METHOD redondeo_condicional.
  DATA: l_entero  TYPE i,
        l_dif     TYPE mengv13,
        l_dif_max TYPE mengv13.

  salida   = cantidad.
  l_entero = cantidad.
  l_dif = ABS( l_entero - cantidad ).

  CHECK l_dif NE 0.

  l_dif_max = diferencia.
  IF NOT porcentaje IS INITIAL.
    l_dif_max = porcentaje * cantidad / 100.
  ENDIF.

  IF l_dif <= l_dif_max.
    salida = l_entero.
  ENDIF.

ENDMETHOD.
