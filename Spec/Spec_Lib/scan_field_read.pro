Function Scan_field_read, snum, field, constant = con, toler = tol, $
	relative = rel, cflag = cfl, list = lis, _extra = _e

;+
; NAME:
;		SCAN_FIELD_READ
; VERSION:
;		7.15
; PURPOSE:
;		Reads fields from a FILDAT structure associated with a SPEC data file.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		Result = SCAN_FIELD_READ( SNUM, FIELD [ keywords])
; INPUTS:
;	SNUM
;		Scan list.  Either an array of integers or a character scalar or array
;		of any form acceptable by SCAN_LIST_VER.  If the single value 0 is
;		provided, it translates to "all scans within the file".
;	FIELD
;		Character scalar, the name of one of the fields in the SCAN
;		substructures (within FILDAT, see common block).  The name must include
;		enough characters for unique identification.  If not provided,
;		SCAN_FIELD_READ will query for it.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/CONSTANT
;		Switch.  If set, SCAN_FIELD_READ checks whether the required field
;		is (are) constant across the provided scan list.  A field is
;		considered constant if the relative difference between its maximal and
;		minimal values (across the list) is smaller than predefined tolerance.
;		This equals to twice the result of TOLER() (see MIDL_LIB) unless the
;		keyword TOLER is used.  For a multi-element field, constancy is checked
;		element by element.
;
;		If CONSTANT is set and the field is indeed constant, its constant
;		value(s) is returned and CFLAG (see below) is set to 1.  Else, the
;		function returns the machine maximum and CFLAG is set to zero.
;	TOLER
;		Tolerance specification, sets the maximal allowed relative difference
;		(for values to be considered equal).  If provided, overrides the
;		the internal setting.
;	/RELATIVE
;		Switch.  If set *explicitly* to 0, TOLER is taken as maximal allowed 
;		*absolute* difference.  Default, as mentioned above, is *relative*.
;	CFLAG
;		Optional output, see below.
;	LIST
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to RANGE_PROC
;		through SCAN_LIST_VER and to SPEC_FILE_INFO.
; OUTPUTS:
;		Returns an array of dimension [NSCAN,NF] where NSCAN is the number of
;		scans in SNUM and NF is the number of elements in the chosen field.
;		If the field is a scalar one, an vector of length NSCAN is returned.
; OPTIONAL OUTPUT PARAMETERS:
;	CFLAG
;		Used in conjunction with the keyword CONSTANT.  Returns 1 if the chosen
;		field is constant, 0 otherwise.  Meaningless if CONSTANT is not set.
;		For multi-element field, CFLAG is a vector of same length as the field.
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
;		Also calls ARREQ, CAST, DEFAULT, STRMATCH_MM, TOLER and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-JUL-2003 by Mati Meron.
;		Modified 15-JUL-2003 by Mati Meron.  Added keywords CONSTANT and CFLAG.
;		Modified 25-JUN-2006 by Mati Meron.  Added keyword LIST.
;		Modified 25-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 15-AUG-2012 by Mati Meron.  Added keyword RELATIVE.
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
			flis = tag_names(fildat.scan)
			case n_elements(field) of
				0	:	begin
							field = ''
							fill = replicate( $
							string(replicate(32b,slen)),n_elements(flis))
							dlis =  strmid(flis + fill,0,16)
							print
							print, dlis, form = '("	",4a)'
							print
							read, field, prompt = 'Which one? '
						end
				1	:
				else:	message, 'One field at a time!'
			endcase
			whi = (Strmatch_mm(field,flis,strlen(field),/all,num=num,/nosub))[0]
			case num of
				0	:	message,"There ain't no such field as "+strupcase(field)
				1	:	begin
							res = [fildat.scan[slis].(whi)]
							q = n_elements(res)/n
							res = reform(transpose(res),n,q)
						end
				else:	message, field + ' is not uniquely defined!'
			endcase
			if keyword_set(con) then begin
				dtol = 2*Toler()
				tol = Default(tol,dtol) > dtol
				cres = total(res,1)/n
				cfl = intarr(q)
				if Default(rel,1,/dtyp) eq 0 then eps = replicate(tol,q) $
				else eps = tol*abs(cres) 
				for i = 0l, q-1 do cfl[i] = $
				(max(res[*,i],min=min) - min) le eps[i]
				typ = Type(res)
				res = Cast(cres,typ,typ)
				dum = where(cfl ne 1,ndum)
				if ndum gt 0 then res[dum] = (machar()).xmax
			endif
			lis = slis
		endif else message, 'No such file or scan(s)!'
	endif else message, 'Scan numbers?'

	return, res
end