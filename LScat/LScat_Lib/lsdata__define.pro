Pro Lsdata__define

;+
; NAME:
;		LSDATA__DEFINE
; VERSION:
;		8.14
; PURPOSE:
;		Defines the {LSDATA} structure used with LS files.
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
;		Standard.  Defines the structure LSDATA, including:
;
;			FILE	:	Name of the "master file", string.
;			STUFF	:	The film material, string.
;			NPRES	:	Number of surface pressures present, long integer.
;			PRVAL	:	Array of pressure values, float, length 32.
;			NORD	:	Number of diffraction orders present, long integer.
;			NSCAN	:	Number of scan files present, long integer.
;			SCAN	:	An array of 128 structures of type LSSCAN.  See
;						LSSCAN__DEFINE for details.
; MODIFICATION HISTORY:
;		Created 25-NOV-2011 by Mati Meron.
;-

	on_error, 1

	dum = {lsdata, file: '', stuff: '', npres: 0l, prval: fltarr(32), nord: 0l,$
	nscan: 0l, scan: replicate({lsscan},128)}

	return
end