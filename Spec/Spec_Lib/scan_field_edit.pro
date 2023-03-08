Pro Scan_field_edit, snum, field, value = val, _extra = _e

;+
; NAME:
;		SCAN_FIELD_EDIT
; VERSION:
;		7.15
; PURPOSE:
;		Writes fields into a FILDAT structure, associated with a SPEC data file.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		SCAN_FIELD_EDIT, SNUM, FIELD [ keywords])
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
;	VALUE
;		The value to written into the selected field within the selected scans.
;		Can be given as vector of same length as the scan list.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to RANGE_PROC
;		through SCAN_LIST_VER and to SPEC_FILE_INFO.
; OUTPUTS:
;		None, other then the appropriate changes in FILDAT.
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		All the scans accessed must be valid scans.
; PROCEDURE:
;		Straightforward, Writes the data using parameters stored in the FILDAT
;		structure in the common block.  Calls SCAN_LIST_VER and SPEC_FILE_INFO.
;		Also calls ARREQ and STRMATCH_MM from MIDL.
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
				1	:	fildat.scan[slis].(whi) = val
				else:	message, field + ' is not uniquely defined!'
			endcase
		endif else message, 'No such file or scan(s)!'
	endif else message, 'Scan numbers?'

	return
end