Pro Source__define

;+
; NAME:
;		SOURCE__DEFINE
; VERSION:
;		8.213
; PURPOSE:
;		Defines the {SOURCE} structure used for the !BLPAR system variable.
; CATEGORY:
;		Initialization.
; CALLING SEQUENCE:
;		None.
; INPUTS:
;		None.
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
;		Standard.  Defines the structure SOURCE, including:
;			SYNC  -	Synchrotron's name.
;			DESC  -	Source description.
;			RENE  -	Ring energy in GeV.
;			RGAM  - The relativistic GAMMA of the machine.
;			CURR  -	The current in the machine in Amperes.
;			BMMF  -	Bending magnet magnetic field, in Tesla.
;			DLEN  -	The standard insertion device length in meters.
;			MGAP  -	Minimal insertion device gap, in mm.
;			RSIG  -	Source (ID) spatial dimensions in mm.  2-element vector
;					in a [sigma_x,sigma_y] format.
;			ASIG  -	Source (ID) angular dimensions in mrad.  2-element vector
;					in a [sigma_x',sigma_y'] format.
;			APER  -	The standard front end aperture size in mm.  2-element
;					vector in a [xsize,ysize] format.
;			DIST  -	The standard distance from source to the aperture in meters.
;		All the numerical values are given in a floating format.
; MODIFICATION HISTORY:
;		Created 20-OCT-2013 by Mati Meron.
;-

	on_error, 1

	dum = {source, sync: '', desc: '', rene: 0., rgam: 0., curr: 0., bmmf: 0., $
	dlen: 0., slen: 0., mgap: 0., sgap: 0., $
	rsig:fltarr(2), asig:fltarr(2), aper:fltarr(2), dist: 0.}

	return
end