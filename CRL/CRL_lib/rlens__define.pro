Pro Rlens__define

;+
; NAME:
;		RLENS__DEFINE
; VERSION:
;		8.73
; PURPOSE:
;		Defines the {RLENS} substructure used for CRL evaluations.
; CATEGORY:
;		Initialization.
; CALLING SEQUENCE:
;		None
; INPUTS:
;		None
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Standard.  Defines the structure RLENS, containing relevant CRL data
;		as follows:
;
;			ELE	:	CRL lens element. Character scalar.
;			DFC	:	Density factor for the lens.  Float scalar.
;			RAD	:	Lens tip radius.  Float scalar.
;			APR	:	Lens aperture.  Float scalar.
;			THI	:	Lens center thickness.  Float scalar.
;			ROF:	Lens roughness.  Float scalar.
; MODIFICATION HISTORY:
;		Created 25-JAN-2022 by Mati Meron.
;-

	on_error, 1

	dum = {Rlens, ele: '',dfc: 0., rad: 0., apr: 0., thi: 0., rof: 0.}

	return
end