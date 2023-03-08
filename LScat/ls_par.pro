Function LS_par, pind, order = ord, parameter = par, $
	horizontal = hor, vertical = ver, dir_sign = drs

;+
; NAME:
;		LS_PAR
; VERSION:
;		8.14
; PURPOSE:
;		Extracts the values of a chosen peak parameter, for a chosen diff. 
;		order and a set of pressures.
; CATEGORY:
;		LS calculations.
; CALLING SEQUENCE:
;		Result = LS_PAR( PIND, ORDER = ORD, PARAMETER= PAR [,optional keywords])
; INPUTS:
; 	PIND
; 		Integer scalar or array, the indices of the required pressures in the 
; 		data set.  Pressures are indexed in ascending order, starting from 1.
; 		PIND value of 0 translates to all the pressures.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	ORDER
; 		Integer scalar in a range of 1-maximal order present.
; 	PARAMETER
; 		String representing one of the 3 peaks Lorentzian parameters.
; 		Possible values are 'Amplitude', 'Center', 'Hwidth'.  Only first 3
;		letters are needed.
; 	/HORIZONTAL
; 		Switch.  If set, data corresponding to horizontal	|	At most one of
; 		measurement is used.								|	these two may be
; 	/VERTICAL												|	used.  If none
; 		Switch.  If set, data corresponding to vertical		|	is, VERTICAL is
; 		measurement is used.  This is the default.			|	assumed.
; 	DIR_SIGN
; 		Integer scalar, only possible values are 1 and -1.  For vertical 
; 		direction 1 corresponds to 'up" and -1 to "down".  For horizontal, 
; 		1 corresponds to "right" and -1 to "left".
; OUTPUTS:
;		Returns a [3,N] array (standard 1D data format) containing:
;			Result[0,*]	-	Pressure values.
;			Result[1,*]	-	Parameter values.
;			Result[2,*]	-	Parameter errors.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		LS_DATA.  Contains LSDAT, a structure of type LSDATA.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		None.
; PROCEDURE:
;		Reads the values from the LSDAT structure.  Calls ARREQ, DEFAULT, 
;		ONE_OF, RANGE_PROC, SIGN and STRMATCH_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-NOV-2011 by Mati Meron.
;-

	common ls_data, lsdat
	on_error, 1
	posib = ['Amplitude','Center','Hwidth']

	wind = Range_proc(pind,/sort,/uni)
	if Arreq(wind,[0]) then wind = 1 + lindgen(lsdat.npres)
	case n_elements(ord) of
		0	:	message, 'Order number must be provided!'
		1	:	if ord le 0 or ord gt lsdat.nord then message, $
				'Invalid order number, available orders are ' + $
				string(lsdat.nord, form = '("1 - ",i0)')
		else:	message, 'A single order number must be provided!'
	endcase

	dir = abs(One_of(hor,ver))
	dsgn = Sign(Default(drs,1))
	if dsgn eq 0 then message, 'DIR_SIGN cannot be 0'
	loc = where(lsdat.scan[0:lsdat.nscan].ord eq ord $
			and lsdat.scan[0:lsdat.nscan].dir eq dir $
			and lsdat.scan[0:lsdat.nscan].dsign eq dsgn, nloc)
	if nloc eq 0 then message, 'No scans of the required type present!'

	res = fltarr(3,nloc)
	res[0,*] = lsdat.scan[loc].pres
	whi = Strmatch_mm(par,posib,3)
	case whi of
		0	:	begin
					res[1,*] = lsdat.scan[loc].amp
					res[2,*] = lsdat.scan[loc].amp_err
				end
		1	:	begin
					res[1,*] = lsdat.scan[loc].cent
					res[2,*] = lsdat.scan[loc].cent_err
				end
		2	:	begin
					res[1,*] = lsdat.scan[loc].hwid
					res[2,*] = lsdat.scan[loc].hwid_err
				end
		else:	message, 'Missing or unrecognizable parameter!
	endcase

	return, res
end