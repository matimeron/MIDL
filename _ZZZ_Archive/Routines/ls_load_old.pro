Pro LS_load_old, file

;+
; NAME:
;		LS_LOAD
; VERSION:
;		8.15
; PURPOSE:
;		Loads processed LS data into a structure.
; CATEGORY:
;		LS calculations.
; CALLING SEQUENCE:
;		LS_LOAD, FILE
; INPUTS:
;	FILE
;		Optional string scalar, the name (full or partial) of an LS master file.
;		See LS_PROCESSS for explanation.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 		None.
; OUTPUTS:
;		Fills up a structure of type LSDATA (see LSDATA__DEFINE), in a common
;		block.  No other output.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		LS_DATA.  Contains LSDAT, a structure of type LSDATA.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		None.
; PROCEDURE:
;		Reads the master file and fills all the fields in LSDAT. 
;		Calls FNAMPARSE, RASCLINE, SORPURGE, STREQ AND STRPARSE_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-NOV-2011 by Mati Meron.
;		Modified 5-APR-2012 by Mati Meron.  Internal changes.
;-

	common ls_data, lsdat
	on_error, 1

	pnt= Rascline(file,buf=2^10,stat=sta,lin=dat,count=nl,call=2,file_name=wfil)
	if sta and nl gt 0 then begin
		blank = 0
		for i = 0, nl-1 do begin
			dum = Strparse_mm(dat[i],' 	')
			if dum eq -1 then blank = blank+1 else if blank eq 2 then break
		endfor
		if dum ne 7 then message, 'Wrong file!'
		if i lt nl then begin
			lsdat = {lsdata}
			lsdat.file = wfil
			fnam = Fnamparse(wfil)
			dum = Strparse_mm(fnam,'	 _',lis)
			lsdat.stuff = lis[0]
			nl = nl - i
			lsdat.nscan = nl
			dat = dat[i:*]
			for j = 1l, nl do begin
				cur = lsdat.scan[j]
				dum = Strparse_mm(dat[j-1],'	 ',lis)
				cur.sfile = lis[1]
				ddum = Strparse_mm(lis[1],'_',llis)
				cur.pres = strmid(llis[1],0,strpos(llis[1],'m'))
				if ddum gt 2 then begin
					fir = strmid(llis[2],0,1)
					if not Streq(fir,'H') then cur.dir = 1
					sec = strmid(llis[2],1,1)
					if (Streq(sec,'D') or Streq(sec,'L')) then cur.dsign = -1 $
					else cur.dsign = 1
				endif else cur.dir = (cur.dsign = 1) 
				cur.ord = llis[ddum]
				for k = 2, 7 do cur.(k+3) = lis[k]
				lsdat.scan[j] = cur
			endfor
			pres = lsdat.scan[1:nl].pres
			pres = pres[Sorpurge(pres,net=npres)]
			lsdat.npres = npres
			lsdat.scan[0].pres = (lsdat.prval[0] = -1.)
			lsdat.prval[1:npres] = pres
			ord = lsdat.scan[1:nl].ord
			ord = ord[Sorpurge(ord,net=nord)]
			lsdat.nord = nord
		endif else message, 'No data in file!'
	endif else message, 'Empty or missing file!'

	return
end