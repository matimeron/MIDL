Pro LS_peaks, pind, horizontal = hor, vertical = ver, lnew = lnw, _extra = _e

;+
; NAME:
;		LS_PEAKS
; VERSION:
;		8.14
; PURPOSE:
;		Displays together LS peaks belonging to same surface pressure.
; CATEGORY:
;		LS calculations.
; CALLING SEQUENCE:
;		LS_PEAKS, PIND [, keywords]
; INPUTS:
; 	PIND
; 		Integer scalar, the index of the required pressure in the data set.  
; 		Pressures are indexed in ascending order, starting from 1.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	/HORIZONTAL												|	At most one of
; 		Switch.  If set, data corresponding to horizontal	|	these two may be
; 		measurement is used.								|	used.  If none
; 	/VERTICAL												|	is, VERTICAL is
; 		Switch.  If set, data corresponding to vertical		|	assumed.
; 		measurement is used.  This is the default.
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
;		needed.  Calls SCAN_SHOW from SPEC.  Calls ARREQ, FNAMPARSE, ONE_OF and
;		TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-NOV-2011 by Mati Meron.
;-

	common ls_data, lsdat
	on_error, 1

	if Type(lsdat) ne 8 or keyword_set(lnw) then LS_load
	snam =  strcompress('s_' + sindgen(8),/remove)

	if n_elements(pind) ne 1 then message, 'Single pressure index required!'
	whi = abs(One_of(hor,ver))
	pres = lsdat.scan[0:lsdat.nscan].pres
	dir = lsdat.scan[0:lsdat.nscan].dir
	prval = lsdat.prval[0:lsdat.npres]
	if pind gt 0 and pind le lsdat.npres then begin
		dum = Fnamparse(lsdat.file,path=path,ext=ext)
		loc = where(pres eq prval[pind] and dir eq whi,nloc)
		if nloc eq 0 then message, 'No scans of the required type present!'
		ord = lsdat.scan[loc].ord
		sgn = lsdat.scan[loc].dsign
		pmfl = (min(sgn,max=max) ne max)
		if pmfl then ncut = nloc/2 else ncut = nloc
		if Arreq(ord,lindgen(nloc) mod ncut + 1) then begin
			for i = 0, nloc-1 do begin
				rfil = path + lsdat.scan[loc[i]].sfile + '_bgs' + ext
				dum = execute(snam[i] + ' = Rascii(rfil,call=2) > 0')
			endfor
			if pmfl then begin
				lcols = [!rainbow[0:ncut-1],!rainbow[0:ncut-1]]
				lines = lonarr(nloc)
				dum = where(sgn lt 0)
				lines[dum] = 1
			endif else lcols = !rainbow
			Scan_show, s_0, s_1, s_2, s_3, s_4, s_5, s_6, s_7, lcol = lcols, $
			tit= lsdat.stuff+ string(prval[pind],form='(", ",f6.2," mN/m")'),$
			xtit = '!7x!x (kHz)', line = lines, _extra = _e
		endif else message, 'Missing orders!'
	endif else message, 'Invalid pressure index, valid range is ' + $
	string(lsdat.npres,form='("1 - ",i0)')

	return
end