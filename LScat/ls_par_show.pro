Pro LS_par_show, pind, parameter = par, $
	horizontal = hor, vertical = ver, dir_sign = drs, _extra = _e

;+
; NAME:
;		LS_PAR_SHOW
; VERSION:
;		8.14
; PURPOSE:
;		Diplays joint plot of the values of one peak parameter for all orders, 
;		as a function of pressure.
; CATEGORY:
;		LS calculations.
; CALLING SEQUENCE:
;		LS_PAR_SHOW, PIND, PARAMETER= PAR [,optional keywords])
; INPUTS:
; 	PIND
; 		Integer scalar or array, the indicis of the required pressures in the 
; 		data set.  Pressures are indexed in ascending order, starting from 1.
; 		PIND value of 0 translates to all the pressures.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	PARAMETER
; 		String representing one of the 3 peaks Lorentzian parameters.
; 		Possible values are 'Amplitude', 'Center', 'Hwidth'.  Only first 3
;		letters are needed.
; 	/HORIZONTAL
; 		Switch.  If set, data corresponding to horizontal	|	At most one of
; 		measurement is used.								|	these two may be
; 	/VERTICAL												|	used.  If none
; 		Switch.  If set, data corresponding to vertical		|	is, VERTICAL is
; 		measurement is used.  This is the default.			|	assumed.
; 	DIR_SIGN
; 		Integer scalar, only possible values are 1 and -1.  For vertical 
; 		direction 1 corresponds to 'up" and -1 to "down".  For horizontal, 
; 		1 corresponds to "right" and -1 to "left".
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
; 		None other than graphics output.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		LS_DATA.  Contains LSDAT, a structure of type LSDATA.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		None.
; PROCEDURE:
;		Gets the data using multiple calls to LS_PAR.  Calls SCAN_SHOW from 
;		SPEC.  Calls STRMATCH_MM and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-NOV-2011 by Mati Meron.
;-

	common ls_data, lsdat
	on_error, 1
	posib = ['Amplitude','Center','Hwidth']
	uni = ['',' (kHz)',' (kHz)']
	whi = Strmatch_mm(par,posib,3)
	
	if Type(lsdat) ne 8 or keyword_set(lnw) then LS_load
	si = strcompress(sindgen(8),/rem)
	snam =  's_' + si

	pre = '= LS_par(pind,ord='
	post = ',par=par,hor=hor,ver=ver,dir=drs)'
	for i = 1, lsdat.nord do dum = execute(snam[i-1] + pre + si[i] + post)

	Scan_show, s_0, s_1, s_2, s_3, s_4, s_5, s_6, s_7, lcol = !rainbow, $
	tit= lsdat.stuff, xtit = 'Pressure (mn/m)', ytit = posib[whi] + uni[whi],$
	_extra = _e

	return
end