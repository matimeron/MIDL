Pro Scan_par_edit, snum, par, value = val, _extra= _e

;+
; NAME:
;		SCAN_PAR_EDIT
; VERSION:
;		7.15
; PURPOSE:
;		Writes parameters into the #P field of a FILDAT structure, associated
;		with a SPEC data file.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		SCAN_PAR_EDIT, SNUM, PAR [,FILNAM [ keywords]])
; INPUTS:
;	SNUM
;		Scan list.  Either an array of integers or a character scalar or array
;		of any form acceptable by SCAN_LIST_VER.  If the single value 0 is
;		provided, it translates to "all scans within the file".
;	PAR
;		Character array (scalar possible as special case) including names of
;		the parameters to be written.  The names must correspond to the names
;		listed in the PP_NAM field of FILDAT (see the common block), including
;		enough characters for unique identification.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	VALUE
;		The value or values to be written into the selected parameters.  Can be
;		given as scalar (in which case all the parameter values selected are
;		replaced with this scalar), a vector of same length as PAR (in which
;		case each parameter, in all the scans, is replaced with the
;		corresponding value) or and array of dimension [N_PAR,N_SNUM].
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to RANGE_PROC
;		through SCAN_LIST_VER AND TO SPEC_FILE_INFO.
; OUTPUTS:
;		None, other then the appropriate changes in FILDAT.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		All the scans accessed must be valid scans.
; PROCEDURE:
;		Straightforward, writes the data using parameters stored in the FILDAT
;		structure in the common block.  Calls SCAN_LIST_VER and SPEC_FILE_INFO.
;		Also calls ARREQ, STRMATCH_MM and STRPARSE_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 5-AUG-2003 by Mati Meron.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	res = 0b
	slen = 16
	n = Scan_list_ver(snum,list=slis,flag=sfl,_extra=_e)
	if n gt 0 and sfl then begin
		Spec_file_info, _extra = _e
		if Arreq(slis,[0]) then begin
			n = fildat.nscan
			slis = 1 + lindgen(n)
		endif
		check = Arreq([slis gt 0 and slis le fildat.nscan],replicate(1b,n))
		if fildat.status and check then begin
			if fildat.npp gt 0 then begin
				p = n_elements(par)
				if p eq 0 then begin
				 	tem = ''
					fill = replicate(string(replicate(32b,slen)),fildat.npp)
					dlis =  strmid(fildat.pp_nam[0:fildat.npp-1] + fill,0,16)
					print
					print, dlis, form = '("	",4a)'
					print
					read, tem, prompt = 'Which one(s)? '
					p = 1 + Strparse_mm(tem,', 	',par)
				endif
				if p gt 0 then begin
					pind = lonarr(p)
					len = strlen(par)
					for i = 0l, p-1 do begin
						pind[i] = (Strmatch_mm(par[i],$
						fildat.pp_nam[0:fildat.npp-1],len[i],/all,num=num))[0]
						if num ne 1 then message, '	"' + par[i] + '"' + $
						 '	is not on the tracked list for this file!', /non
					endfor
					fildat.scan[slis].pp_set[pind] = 1
					fildat.scan[slis].pp_val[pind] = val
					fildat.pp_exs= fildat.pp_exs or fildat.scan[slis[0]].pp_set
				endif else message, 'Which parameter(s)?'
			endif else message, 'No parameters defined for this file!'
		endif else message, 'No such file or scan(s)!'
	endif else message, 'Scan numbers?'

	return
end