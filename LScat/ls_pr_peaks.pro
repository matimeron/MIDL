Pro LS_pr_peaks, pind, order= ord, horizontal= hor, vertical= ver, sign= sgn, $
	center = cnt, scale = scl, lnew = lnw, _extra = _e

;+
; NAME:
;		LS_PR_PEAKS
; VERSION:
;		8.15
; PURPOSE:
;		Displays together LS peaks belonging to same diffraction order.
; CATEGORY:
;		LS calculations.
; CALLING SEQUENCE:
;		LS_PR_PEAKS, PIND [, keywords]
; INPUTS:
; 	PIND
; 		Integer scalar, or vector, in any form expandable by RANGE_PROC (see 
; 		there), representing the indices of the required pressured in the data 
; 		set.  Pressures are indexed in ascending order, starting from 1.  If 
; 		not given, defaults to all the pressures in the data set.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ORDER
;		Integer scalar specifying the diffraction order to be used.  Must be 
;		from the range of orders present in the data.  Default value is 1.
; 	/HORIZONTAL												|	At most one of
; 		Switch.  If set, data corresponding to horizontal	|	these two may be
; 		measurement is used.								|	used.  If none
; 	/VERTICAL												|	is, VERTICAL is
; 		Switch.  If set, data corresponding to vertical		|	assumed.
; 		measurement is used.  This is the default.
; 	SIGN
; 		Specifies the sign (positive or negative) for data sets where both signs
; 		are present for a given direction.  Should be given as +-1 (though any
; 		positive or negative value will do).  Default is positive.
; 	/CENTER
; 		Switch.  If set, all the curves are offset by the values of the center 
; 		of their corresponding fits, bringing them to a common origin.
; 	/SCALE
; 		Switch.  If set, all the curves are scaled by the the values of the 
; 		amplitudes of their corresponding fits, bringing them to a common height
; 	/LNEW
; 		Switch.  If set, prompts the loading of new file data.  This also 
; 		happens automatically if no file has been loaded.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to be
;		used directly.
; OUTPUTS:
;		None other than screen output.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		LS_DATA.  Contains LSDAT, a structure of type LSDATA.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		None.
; PROCEDURE:
;		Uses LSDAT to locate the data corresponding to PIND and the keywords,
;		loads the data from the files and displays it.  Calls LS_LOAD, if 
;		needed.  Calls SCAN_SHOW from SPEC.  Calls DEFAULT, FNAMPARSE, 
;		LEGEND_MM, ONE_OF, RANGE_PROC, SIGN and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-APR-2012 by Mati Meron.
;-

	common ls_data, lsdat
	on_error, 1

	if Type(lsdat) ne 8 or keyword_set(lnw) then LS_load
	snam =  strcompress('s_' + sindgen(16),/remove)

	if n_elements(pind) gt 0 then wind = Range_proc(pind) $
	else wind = 1 + lindgen(lsdat.npres)
	dum = where(wind ge 1 and wind le lsdat.npres, npr)
	if npr eq 0 then message, 'Unacceptable pressure list, valid range is ' + $
	string(lsdat.npres,form='("1 - ",i0,"!")') else wind= wind[dum]
	prval = lsdat.prval[wind]
	word = (Default(ord,1,/dtyp))[0]
	if word lt 1 or word gt lsdat.nord then message, $
	'Invalid order, valid range is ' + string(lsdat.nord,form='("1 - ",i0,"!")')
	wdir = abs(One_of(hor,ver))
	wsgn = Sign(Default(sgn,1))

	plis = lsdat.scan[0:lsdat.nscan].pres
	vlis = lonarr(lsdat.nscan + 1)
	for i = 0l, npr - 1 do begin
		dum = where(plis eq prval[i], ndum)
		if ndum gt 0 then vlis[dum] = 1
	endfor
	wind = where(vlis)
	wdat = lsdat.scan[wind]

	dum= where(wdat.ord eq word and wdat.dir eq wdir and wdat.dsign eq wsgn,nsc)
	if nsc gt 0 then wdat = wdat[dum] else message, 'No matching scans!'

	dum = Fnamparse(lsdat.file,path=path,ext=ext)
	for i = 0l, nsc - 1 do begin
		rfil = path + wdat[i].sfile + '_bgs' + ext
		dum = execute(snam[i] + ' = Rascii(rfil,call=2) > 0')
	endfor
	if keyword_set(cnt) then hof = - wdat.cent 
	if keyword_set(scl) then sca = 1/wdat.amp

	tit= lsdat.stuff +'!c'+$
	strjoin(strcompress(string(prval,form='(f5.1)'),/rem),',') + ' (mN/m)'
	stit = (['Horizontal','Vertical'])[wdir] + ' direction ; ' + $
	'Order = ' + string(word,form='(i0)')

	Scan_show, s_0, s_1, s_2, s_3, s_4, s_5, s_6, s_7, $
	s_8, s_9, s_10, s_11, s_12, s_13, s_14, s_15, $
	hof = hof, sca = sca, /part, lcol = !rainbow, xmarg =[8,5], ymarg =[6,4], $
	ext = 0.3, tit = tit, subtit = stit, xtit = '!7x!x (kHz)', _extra = _e

	Legend_mm, line=0, text = string(wdat.hwid,form='(f5.2)'), $
	col= ([!rainbow,!rainbow])[0:nsc-1] 

	return
end