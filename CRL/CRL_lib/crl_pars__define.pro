Pro CRL_pars__define

;+
; NAME:
;		CRL_PARS__DEFINE
; VERSION:
;		8.73
; PURPOSE:
;		Defines the {CRL_PARS} structure used for CRL evaluations.
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
;		Standard.  Defines the structure CRL_PARS, containing relevant CRL data
;		as follows:
;
;			STATION :	Station name. Character scalar.
;			SOL		:	Source location. Float scalar.
;			OPL		:	Optics (CRL) location.  Float scalar.
;			SAL		:	Sample location.  Float scalar.
;			MIFL	:	Mirror Flag.  Integer scalar.
;			MIL		:	Mirror location.  Float scalar.
;			SLE		:	Mirror slope error.  Float scalar.
;			LENSET	:	Number of lens sets used.  Integer scalar.
;			LENMAX	:	Maximal number of lenses.  Integer scalar.
;			LEN		:	RLENS type structure.
;			XLEN	:	RLENS type structure.
;
;		Note:	All the locations are relative to the beamline Center of
;		Straight Section (CSS).
; MODIFICATION HISTORY:
;		Created 25-JAN-2022 by Mati Meron.
;-

	on_error, 1

	dum = {CRL_pars, station: '', $
	sol: 0., opl: 0., sal: 0., mifl: 0, mil: 0., sle: 0., lenset: 0, lenmax: 0,$
	len: {rlens}, xlen: {rlens}}

	return
end