Function K_gen, lam = lam, energy = ene, k = kvl, type = typ, single = sng

;+
; NAME:
;		K_GEN
; VERSION:
;		8.47
; PURPOSE:
;		Converting from energy or wavelength to K.
; CATEGORY:
;		X-ray specific.
; CALLING SEQUENCE:
;		Result = K_GEN ( LAM= LAM, ENERGY= ENE, K= KVL [ optional keywords])
; INPUTS:
; 		None.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
;	LAM														|
;		Scalar, the value of the wavelength in Angstrem.	| One and only
;	ENE														| one of these
;		Scalar, the energy value in keV.					| three must be
;	K														| defined.
;		Scalar, the K value in inverse Angstrem.			|
; 	TYPE
;		Integer scalar, the requested numeric type of the output.  If not given
;		the output type will be the higher of the input types.  In any case the
;		output type will be no lower than 4 (float).
;	/SINGLE
;		Switch.  If set, the input (thus also the output) must be a scalar.  If
;		it is not, an error message will be generated.
; OUTPUTS:
;		Returns the K-value(s) corresponding to the input.  If the input was
;		K, it is returned as is (other than possible type conversion).
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The Q values must be physically possible.
; PROCEDURE:
;		Straightforward.  Calls CALCTYPE, CAST, DEFAULT and ONE_OF, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-JUL-2016 by Mati Meron.
;-

	on_error, 1

	rtyp = Default(typ,Calctype(lam,ene,kvl,0.,def=4),/dtyp) > 4

	case One_of(lam,ene,kvl,val=lval) of
		0	:	k = 2*!dpi/lval
		1	:	k = 2*!dpi*lval/!srcon.conv
		2	:	k = Cast(lval,5)
		else:	message,'Missing energy/wavelength/k input!'
	endcase

	if keyword_set(sng) then if n_elements(k) eq 1 then k = k[0] $
	else message, 'energy/wavelength/k must be a scalar!'

	return, Cast(k,rtyp,rtyp)
end