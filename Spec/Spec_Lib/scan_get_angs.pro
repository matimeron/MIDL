Function Scan_get_angs, snum, fnum, delta = del, full = ful, $
	first = fir, mean = mea, radians = rad, warn = wrn, cflag = cfl

;+
; NAME:
;		SCAN_GET_ANGS
; VERSION:
;		8.01
; PURPOSE:
;		Returns the values of the angles ALPHA, BETA and DTH for a given scan.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		Result = SCAN_GET_ANGS( SNUM [, FNUM] [, keywords])
; INPUTS:
;	SNUM
;		Single scan number.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Frame number(s) within the scan.  If not given, defaults to all frames.
; KEYWORD PARAMETERS:
;	DELTA
;		Numeric value, the smallest angle difference that is recognized as
;		"true difference".
;	/FULL
;		Switch, specifies output data format.  Usually the output is a 3-element
;		vector, with one entry per angle.  If /FULL is set, the output becomes a
;		[3,N] array where N is the number of frames (all if not specified) used.
;		In this case each *column* of the result corresponds to one angle.
;	/FIRST															|
;		Switch, specifies action to be taken if one of the angles	| One and
;		is present not only in the parameter list but also as a 	| only one
;		scan variable (corresponding to a column in the data block	| of these
;		in the scan).  In this case, if /FIRST is set and /FULL is	| two may
;		not, the entry corresponding to this angle is replaced by 	| be used.
;		the element corresponding to the first value of FNUM, in 	|
;		said column.												| Default
;	/MEAN															| is /FIRST
;		Switch, same as /FIRST, only the replacement value is taken	|
;		from the mean of the column, instead of the first element.	|
;
;		Note:	If /FULL is set, /FIRST and /MEAN have no effect.
;	/RADIANS
;		Switch.  If set, the angles are output in radians.  Default is degrees.
;	/WARN
;		Switch, if set and columns corresponding to the angles exist, a warning
;		is issued if one of the angles corresponds to a column and the column is
;		not constant.
;	CFLAG
;		Optional output, see below.
; OUTPUTS:
;		Returns the values of the key angles (see above) for the scan.
;
;		Note:	In the case of "far detector" all the angles other than ALPHA
;				are returned as 0.
; OPTIONAL OUTPUT PARAMETERS:
;	CFLAG
;		Returns constancy checking vector values.  An entry of 1 means that the
;		corresponding angle is constant within the scan.  0 means it is not.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data files must exist.
; PROCEDURE:
;		Straightforward, reads the angles using parameters stored in the FILDAT
;		structure in the common block.  Calls SCAN_COLUMN and SCAN_PD_FRAMES.
;		Also calls DEFAULT, ONE_OF and RANGE_PROC, from MIDL.
; MODIFICATION HISTORY:
;		Created 20-JUN-2008 by Mati Meron.
;		Modified 15-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 5-FEB-2011 by Mati Meron.  Far detector support.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	ddel = 1e-3
	wdel = Default(del,ddel,/dtyp) > ddel
	if keyword_set(rad) then mult = !dtor else mult = 1
	unam = ['Alpha','Beta','Dth']
	anum = n_elements(unam)

	wsnum = Range_proc(snum)
	if n_elements(wsnum) gt 1 then message, 'Only single scan allowed!'
	wfnum = Scan_PD_frames(wsnum,fnum,nfr=nfnum)
	cur = fildat.scan[wsnum]
	angs = (cur.angs)[0:anum-1]
	ncfl = (cur.varan)[0:anum-1]

	wrn = Default(wrn,intarr(anum),/dtyp)
	if n_elements(wrn) eq 1 then wrn = replicate(wrn,anum) $
	else if n_elements(wrn) ne anum then message, 'Invalid warning codes!'

	fulfl = keyword_set(ful)
	if fulfl then begin
		res = fltarr(anum,nfnum)
		for i = 0, anum-1 do res[i,*] = angs[i]
	endif else res = angs

	var = where(ncfl,nvar)
	if nvar gt 0 then begin
		cval = (Scan_column(wsnum,0))[wfnum]
		if fulfl then res[var,*] = cval else $
		if not One_of(mea,fir) then res[var] = total(cval)/nfnum
		if (max(cval,min=min) - min) ge wdel then begin
			if wrn[var] then message, unam[var] + $
			' is not constant in scan # ' + string(wsnum,form='(i0,"!")'), /con
		endif else ncfl[var] = 0
	endif
	cfl = 1 - ncfl
	if cur.pdfar then res[1:2,*] = 0 

	return, mult*res
end