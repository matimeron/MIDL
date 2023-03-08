Pro LS_info, lnew = lnw

;+
; NAME:
;		LS_INFO
; VERSION:
;		8.14
; PURPOSE:
;		Provides basic info about an LS file group.
; CATEGORY:
;		LS calculations.
; CALLING SEQUENCE:
;		LS_INFO [, LNEW = LNW]
; INPUTS:
; 		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	/LNEW
; 		Switch.  If set, prompts the loading of new file data.  This also 
; 		happens automatically if no file has been loaded.
; OUTPUTS:
;		None other than screen output.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		LS_DATA.  Contains LSDAT, a structure of type LSDATA.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		None.
; PROCEDURE:
;		Displays to the screen basic info from the structure LSDAT. 
;		Calls LS_LOAD, if needed.  Calls TYPE and TABULATE, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-NOV-2011 by Mati Meron.
;-

	common ls_data, lsdat
	on_error, 1

	if Type(lsdat) ne 8 or keyword_set(lnw) then LS_load

	print
	print, lsdat.stuff, lsdat.nscan, lsdat.nord, $
	form = '("  ",a,", ",i0," scans, orders: 1-",i0)'
	print
	pres = lsdat.prval[1:lsdat.npres]
	Tabulate, pres,/ind,/fir,tit= 'Pressures',form= 'f6.2', head= 'Pres. (mN/m)'
	print 

	return
end