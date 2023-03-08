Pro Whazzup, ldignore = ldig,  _extra = _e

;+
; NAME:
;		WHAZZUP
; VERSION:
;		7.15
; PURPOSE:
;		Checks SPEC files for potential data problems.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		WHAZZUP [,keywords]
; INPUTS:
;		None
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/LDIGNORE
;		Switch.  Instructs WHAZZUP to ignore the "column overrun" problem which
;		occurs only with Linear Detector scans and is not important.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to
;		SPEC_FIELD_READ.  Not to be used directly.
; OUTPUTS:
;		None other than screen printout.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Accesses the SPEC file data structure currently in memory to check the
;		STATUS field for all scans.  Prints out a list of those with problems.
;		Calls SCAN_FIELD_READ from SPEC and TABULATE from MIDL.
; MODIFICATION HISTORY:
;		Created 15-AUG-2003 by Mati Meron.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	mess = ['Nonexistant scan','Scan OK','No data present',$
			'Data exists but may be incomplete','',$
			'Data_header - data mismatch','','Some illegitimate data present']
	fill = string(replicate(32b,36))

	if keyword_set(ldig) then ign = 5 else ign = 1
	inf = Scan_field_read(0,'stat',_extra=_e)
	dum = where(inf ne 1 and inf ne ign, ndum)
	print
	print, '	File:   ' + fildat.name
	print
	print, '	' + strcompress('Last: scan # ' + string(fildat.nscan)) + $
		';	' + fildat.scan[fildat.nscan].sdatime
	print
	if ndum gt 0 then Tabulate, fix(dum+1),mess[inf[dum]] + fill, form=['i4','a36'],$
	head = ['scan #','status'+fill], tit = 'Status report' else print, '	Nothing'
	print

	return
end