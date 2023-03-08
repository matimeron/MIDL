Pro Bimorph_stat, snum

;+
; NAME:
;		BIMORPH_STAT
; VERSION:
;		8.15
; PURPOSE:
;		Provides information about bimorph mirror scans..
; CATEGORY:
;		Bimorph mirror specific.
; CALLING SEQUENCE:
;		Result = BIMORPH_STAT, SNUM
; INPUTS:
;	SNUM
;		Scan number or a list of scan numbers, in any form acceptable by
;		SCAN_LIST_VER.  If not given or set to 0, translates to "all scans in 
;		the file".
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 		None.
; OUTPUTS:
; 		Displays to the screen a table of information about the bimorph scan 
; 		found within the list SNUM.  The printed columns, from left to right, 
; 		are:
; 			Scan number		:	
; 			Mirror settings :	16	-	All the voltages are 0
; 								0-15-	Single non-zero voltage, the number
; 										indicates the segment.
; 								17	-	multiple non-zero voltages.
; 			Voltage value	:	0 for all voltages 0, the non-zero value for
; 								single non-zero voltage, 10000 otherwise.
; 			Number of points:
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		SPEC_FILE.  See SPEC_FILE_INFO for details.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
; 		Straightforward, calling BIMORPH_VER.
; MODIFICATION HISTORY:
;		Created 15-JAN-2012 by Mati Meron.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	lis = Bimorph_ver(snum,nsc=nsc)
	for i = 0, nsc - 1 do begin
		vals = fildat.scan[lis[i]].mirval
		check = vals ne 0
		dum = where(check, ndum)
		case ndum of
			0	:	begin
						whi = 16l
						val = 0.
					end
			1	:	begin
						whi = dum[0]
						val = vals[dum[0]]
					end
			else:	begin
						whi = 17l
						val = 1e4
					end
		endcase
		npo = (fildat.scan[lis[i]].ncr)[1]
		print, lis[i], whi, val, npo
	endfor

	return
end