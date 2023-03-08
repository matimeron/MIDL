Pro Spec_field_edit, field, value = val, _extra = _e

;+
; NAME:
;		SPEC_FIELD_EDIT
; VERSION:
;		7.15
; PURPOSE:
;		Writes fields into a FILDAT structure, associated with a SPEC data file.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		SPEC_FIELD_EDIT, SNUM, FIELD [ keywords])
; INPUTS:
;	SNUM
;	FIELD
;		Character scalar, the name of one of the fields in global part of
;		FILDAT.  The name must include enough characters for unique
;		identification.  If not provided, SPEC_FIELD_EDIT will query for it.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	VALUE
;		The value to written into the selected field.  If the field is a vector,
;		a vector value (of length not exceeding the length of the field) can be
;		given.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to SPEC_FILE_INFO
; OUTPUTS:
;		None, other then the appropriate changes in FILDAT.
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, Writes the data using parameters stored in the FILDAT
;		structure in the common block.  Calls SPEC_FILE_INFO.  Also calls ARREQ
;		and STRMATCH_MM from MIDL.
; MODIFICATION HISTORY:
;		Created 10-NOV-2007 by Mati Meron.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	slen = 16
	Spec_file_info, _extra = _e
	if fildat.status then begin
		flis = tag_names(fildat)
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
			1	:
			else:	message, field + ' is not uniquely defined!'
		endcase
		if n_elements(val) eq 0 then begin
			tval = ''
			print
			print, 'Current value(s):'
			print, fildat.(whi)
			print
			read, tval, prompt = 'New value(s)?'
			if not Arreq(tval,'') then val = tval
		endif
		nval = n_elements(val)
		nfld = n_elements(fildat.(whi))
		if nval gt 0 then begin
			if nval le nfld then fildat.(whi) = val $
			else message, 'Too many values!'
		endif
	endif else message, 'Bad or nonexistant file!'

	return
end