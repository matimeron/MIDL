Function Check_struct, struct, ind, recursive = rec, detailed = det

;+
; NAME:
;		CHECK_STRUCT
; VERSION:
;		8.73
; PURPOSE:
;		Verifying initialized (non zero) values in a structure.
; CATEGORY:
;		Programming Utility.
; CALLING SEQUENCE:
;		Result = CHECK_STRUCT( STRUCT, IND [, RECURSIVE = REC] [, DETAILED= DET]
; INPUTS:
;	STRUCT
;		An arbitrary structure. Mandatory.
;	IND
;		Search indices.  Provided either as an integer type array, containing
;		the indices of the tags to be checked, or as a character array,
;		containing the appropriate tag names.  If not provided, it defaults to
;		"all the tags".
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/RECURSIVE
;		Switch.  Normally only the top layer of the structure is checked, any
;		fields that are structures themselves are assumed to be initialized
;		with no checking.  Setting /RECURSIVE forces a recursive check of all
;		the substructures, down to any level.
;		Note: In case of a recursive check, all the fields of the substructures
;		are checked, regardless of the settings of IND.
;	DETAILED
;		Optional output, see below.
; OUTPUTS:
; 		If all the checked fields contain some non-zero values or, in case of
; 		character type fields, some non-null string values, returns 1.  Else,
; 		returns zero,
; OPTIONAL OUTPUT PARAMETERS:
;		In case some fields didn't pass the test, i.e. are all zero or all null
;		string, returns the indices of those fields or their tag names,
;		depending on whether IND contains indices or tag names.  Else, returns
;		!NULL.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls SORPURGE, TYPE and WHERINSTRUCT, from MIDL. May
;		call itself, recursively.
; MODIFICATION HISTORY:
;		Created 10-MAR-2022 by Mati Meron.
;-

	on_error, 1

	tag = tag_names(struct)
	nt = n_tags(struct)
	if nt gt 0 then begin
		cfl = 1
		typ = Type(ind)
		case typ of
			0	:	wind = lindgen(nt)
			7	:	begin
						n = n_elements(ind)
						wind = lonarr(n)
						for i = 0, n-1 do wind[i] = $
						Wherinstruct(ind[i],struct,/exact)
					end
			else:	wind = long(ind[Sorpurge(ind)])
		endcase
		dum = where(wind ge 0 and wind lt nt, nn)
		if nn gt 0 then begin
			wind = wind[dum]
			wtag = tag[wind]
			rfl = keyword_set(rec)
			ires = lonarr(nn)
			for j = 0, nn-1 do begin
				tem = struct.(wind[j])
				case Type(tem) of
					7	:	ires[j] = max(tem) ne ''
					8	:	begin
								if rfl then begin
									dum = execute('sstruct = struct.'+wtag[j])
									ires[j] = Check_struct(sstruct,/rec)
								endif else ires[j] = 1
							end
					else:	ires[j] = max(abs(tem)) ne 0
				endcase
			endfor
		endif else message, 'No valid tags!'
	endif else message, 'missing structure input!

	res = product(ires,/pres)
	if arg_present(det) and not res then begin
		k = where(ires eq 0)
		det = wind[k]
		if typ eq 7 then det = wtag[det]
	endif else det = []

	return, res
end