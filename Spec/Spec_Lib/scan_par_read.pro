Function Scan_par_read, snum, par, zero_sub= zsb, exists= pex, constant= con,$
	toler = tol, cflag = cfl, list = lis, _extra= _e

;+
; NAME:
;		SCAN_PAR_READ
; VERSION:
;		7.15
; PURPOSE:
;		Reads parameters (from the #P field) from a FILDAT structure
;		associated with a SPEC data file.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		Result = SCAN_PAR_READ( SNUM, PAR [, keywords])
; INPUTS:
;	SNUM
;		Scan list.  Either an array of integers or a character scalar or array
;		of any form acceptable by SCAN_LIST_VER.  If the single value 0 is
;		provided, it translates to "all scans within the file".
;	PAR
;		Character array (scalar possible as special case) including names of
;		the parameters to be read.  The names must correspond to the names
;		listed in the PP_NAM field of FILDAT (see the common block), including
;		enough characters for unique identification.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/ZERO_SUB
;		Switch.  If set and if some of the parameters in PAR are not defined
;		for all the scans in SNUM, zeros are substituted for the missing values.
;		Default actions is error message, no substitution.
;	EXISTS
;		Optional output, see below.
;	/CONSTANT
;		Switch.  If set, SCAN_PAR_READ checks whether the required parameter(s)
;		is (are) constant across the provided scan list.  A parameter is
;		considered constant if the relative difference between its maximal and
;		minimal values (across the list) is smaller than predefined tolerance.
;		This equals to twice the result of TOLER() (see MIDL_LIB) unless the
;		keyword TOLER is used.
;
;		If CONSTANT is set and the parameter is indeed constant, the constant
;		scalar value is returned and CFLAG (see below) is set to 1.  Else, the
;		function returns the machine maximum and CFLAG is set to zero.
;	TOLER
;		Tolerance specification, sets the maximal allowed difference
;		(for values to be considered equal).  If provided, overrides the
;		the internal setting.  Can be given as a vector, in which case each
;		value sets the tolerance for the corresponding parameter.  If given as
;		vector, zero or missing values are replaced by the internal setting
;		(which is twice the machine maximum).
;	CFLAG
;		Optional output, see below.
;	LIST
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to RANGE_PROC
;		through SCAN_LIST_VER and to SPEC_FILE_INFO.
; OUTPUTS:
;		Returns an array of dimension [NSCAN,NPAR] where NSCAN is the number of
;		scans in SNUM and NPAR is the number of parameters in PAR.  However, if
;		CONSTANT is set, the function returns a vector of length NPAR, with each
;		entry being the constant value of the corresponding parameter.
; OPTIONAL OUTPUT PARAMETERS:
;	EXISTS
;		An integer vector with same length as PAR.  On return, each entry equals:
;			0 	:	if corresponding parameter is listed but not defined.
;			1	:	if corresponding parameter is defined everywhere.
;			2	:	if corresponding parameter is defined, but not everywhere.
;
;		Note:	Unless ZERO_SUB is set, any outcome but 1 results in error.
;	CFLAG
;		An integer vector with same length as PAR.  On return, each entry equals:
;			0 	:	if corresponding parameter is not constant
;			1	:	if corresponding parameter is constant.
;
;		If /CONSTANT is not set, CFLAG is meaningless.
;		Note:	Only paramaters which are defined everywhere can qualify as
;		constant.  Therefore, if for a given parameter EXISTS returns result
;		different from 1, then for same parameter CFLAG returns zero.
;	LIST
;		Returns the expanded list of all the scan numbers, as provided by SNUM.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		All the scans accessed must be valid scans.
; PROCEDURE:
;		Straightforward, reads the data using parameters stored in the FILDAT
;		structure in the common block.  Calls SCAN_LIST_VER and SPEC_FILE_INFO.
;		Also calls ARREQ, CAST, DEFAULT, STRMATCH_MM, STRPARSE_MM, TOLER and 
;		TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 1-APR-2003 by Mati Meron.
;		Modified 10-JUL-2003 by Mati Meron.  Added keywords ZERO_SUB and EXISTS.
;		Modified 15-JUL-2003 by Mati Meron.  Added keywords CONSTANT and CFLAG.
;		Modified 25-JUN-2006 by Mati Meron.  Added keyword LIST.
;		Modified 25-MAR-2010 by Mati Meron.  Internal changes.
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
			pdef = where(fildat.pp_exs,npdef)
			if npdef gt 0 then begin
				p = n_elements(par)
				if p eq 0 then begin
				 	tem = ''
					fill = replicate(string(replicate(32b,slen)),npdef)
					dlis =  strmid(fildat.pp_nam[pdef] + fill,0,16)
					print
					print, dlis, form = '("	",4a)'
					print
					read, tem, prompt = 'Which one(s)? '
					p = 1 + Strparse_mm(tem,', 	',par)
				endif
				if p gt 0 then begin
					zfl = keyword_set(zsb)
					pex = intarr(p)
					pind = lonarr(p)
					len = strlen(par)
					for i = 0l, p-1 do begin
						pind[i] = (Strmatch_mm(par[i],$
						fildat.pp_nam[0:fildat.npp-1],len[i],/all,num=num))[0]
						if num ne 1 then message, '	"' + par[i] + '"' + $
						 '	is not on the tracked list for this file!', /non
					endfor
					pex = fildat.pp_exs[pind]
					if not zfl then begin
						dum = where(pex eq 0, ndum)
						if ndum gt 0 then message, strjoin(par[dum],', ') + $
						string(13b) + '	not defined in this file!', /non
					endif
					ver =reform(transpose([fildat.scan[slis].pp_set[pind]]),n,p)
					dum = where(total(ver,1) ne n, ndum)
					if ndum gt 0 then begin
						pex[dum] = 2*pex[dum]
						if not zfl then message, strjoin(par[dum],', ') + $
						string(13b) + '	not everywhere defined!', /non
					endif
					res= reform(transpose([fildat.scan[slis].pp_val[pind]]),n,p)
					if keyword_set(con) then begin
						dtol = 2*Toler()
						wtol = Default(tol,dtol) > dtol
						if n_elements(wtol) eq 1 then wtol = replicate(wtol,p) $
						else wtol = ([wtol,replicate(dtol,p)])[0:p-1]
						cres = total(res,1)/n
						cfl = intarr(p)
						for i = 0l, p-1 do cfl[i] = $
						(max(res[*,i],min=min) - min) le wtol[i]
						cfl = cfl*pex
						typ = Type(res)
						res = Cast(cres,typ,typ)
						dum = where(cfl ne 1,ndum)
						if ndum gt 0 then res[dum] = (machar()).xmax
					endif
					lis = slis
				endif else message, 'Which parameter(s)?'
			endif else message, 'No parameters in this file!'
		endif else message, 'No such file or scan(s)!'
	endif else message, 'Scan numbers?'

	return, res
end