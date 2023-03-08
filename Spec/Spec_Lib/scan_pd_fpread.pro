Function Scan_PD_fpread, snum, fnum, read_new = rnw, verify = ver, bad = bad, $
	orientation = ori, outmode = otm, rotate = rot, hroi = hroi, vroi = vroi, $
	foff = fof, grid_corr = grc, norm = nrm, globnorm = glb, $
	nfact= wco, coord= cor, flist= wfnum, nframes= nfnum, title= tit, _extra= _e

;+
; NAME:
;		SCAN_PD_FPREAD
; VERSION:
;		8.476
; PURPOSE:
;		Reads Pilatus Detector files.
; CATEGORY:
;		SPEC Input/Output.
; CALLING SEQUENCE:
;		Result = SCAN_PD_FPREAD( SNUM [, FNUM] [, keywords])
; INPUTS:
;	SNUM
;		Single scan number, mandatory.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Frame number within the scan.  If not given, defaults to all frames.
;		A value of -1 is equivalent to "not given", useful for programming.
; KEYWORD PARAMETERS:
; 	/READ_NEW
; 		Switch.  If set, new data is read, else previously read data is reused, 
; 		subject to the following limitations:
; 			1)	Previous data must exist, i.e. SCAN_PD_FPREAD must've been 
; 				already called before.
; 			2)	The previous data must correspond to the same scan and frame
; 				numbers as the current call.
; 		
; 		Note:	While reusing previous data, it is possible to change regions
; 				of interest, normalization mode, BAD settings etc.  On the 
; 				other hand the setting of VERIFY (see below) should not be 
; 				changed.
;	/VERIFY
;		Switch.  If set, the existence of all frames to be processed is
;		verified and bad or missing frames are rejected.
;	/BAD
;		Switch.  If set, faulty (very high count rate) pixels are removed from
;		the data.  If bad is provided as integer > 1, the removal process is 
;		repeated the number of times specified by the value given.
;	ORIENTATION
;		An optional input specifying the detector orientation.  Can be provided
;		as integer input, 0 for horizontal, 1 for vertical.  Alternatively, can
;		also be provided as character input with two possible values, "HOR"
;		(for horizontal) and "VER" (for vertical).  First letter is sufficient.
;		Default is vertical.
;	OUTMODE
;		An optional input specifying the detector column arm used.  Can be
;		provided as integer input, 0 for the "XR" arm, 1 for "GID".
;		Alternatively can be provided as character input with two possible
;		values, "XR" and "GID".  Default is "GID".
;
;		Note:	The default values of these two values are taken from the
;		assigned SPEC file, if one exists.
;
;		Note:	Both keywords above also serve as optional outputs, returning
;		the orientation and detector column arm values actually used.
;	ROTATE
;		Integer scalar, specifies rotation as in the IDL ROTATE function.
;		Optional, default is established internally.
;	HROI
;		Two element vector defining horizontal region of interest, in *pixels*.
;	VROI
;		Two element vector defining vertical region of interest, in *pixels*.
;	FOFF
;		Numeric array (even number of entries) providing offsets for individual
;		frames.  The offsets apply to HROI and VROI, if neither of these two is
;		present, FOFF has no effect.
;	GRID_CORR
;		Specifies correction for the grid present in Pilatus detector data.  If
;		given as a value such that |GRID_CORR| < 1, the data within the grid
;		pixels is divided by (1 + GRID_CORR).  If given as 1 (or set as 
;		/GRID_CORR), the last used value (0 if non was used) is reused.  If not
;		given or set, no correction is performed.
;	/NORM
;		Switch.  If set, the data is normalized to monitor counts, scaled by
;		the average of monitor counts for the scan.  The scaling can be changed,
;		see keyword /GLOBNORM, below.
;	/GLOBNORM
;		Switch.  If set, the normalization is "global", i.e. to the value of
;		MONC alone, with no scaling.  This yields faithful "global
;		normalization" across multiple scans, at the price of the data not being
;		approximately equivalent to numbers of counts.
;	NFACT
;		Optional output, see below.
;	COORD
;		Optional output, see below.
;	FLIST
;		Optional output, see below.
;	NFRAMES
;		Optional output, see below.
;	TITLE
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.
; OUTPUTS:
;		Returns an array (type FLOAT) of dimension [NFR,PX,PY] where NFR is the
;		number of frames in SNUM and PX, PY are the Pilatus horizontal (short)
;		and vertical (long) pixel sizes (if "HOR" orientation is used, the
;		dimensions are reversed).  Each page of the array contains the raw data
;		corresponding to one of the scans in FNUM.
; OPTIONAL OUTPUT PARAMETERS:
; 	NFACT
; 		Returns the multiplicative normalization factors applied to the frames.
;	COORD
;		Returns a [2,PX,PY] array contining, in order, the horizontal and
;		vertical coordinates of the image.
;	FLIST
;		Returns the list of frames for which the evaluation has been done.
;	NFRAMES
;		Returns the number of frames for which the evaluation has been done.
;	TITLE
;		Returns the Pilatus data filename to be used as a plot title, for the
;		use of calling routines.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
;		PGRID_CORR.  Holds the Pilatus grid correction value and the 
;		corresponding correction array.
;		PREV_READ.  Contains the following:
;			LSNUM	-	Scan number from last call.
;			LDIM	-	Dimensions of the last read data.
;			LWFNUM	-	Numbers of the frames read in last call.
;			LRES	-	The raw (i.e. before the application of normalization,
;						regions of interest etc.) data read in the last call.	
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data file must exist.
; PROCEDURE:
;		Straightforward, reads the data using parameters stored in the FILDAT
;		structure in the common block.  Calls IMG_EXORCISE, SCAN_COLUMN,
;		SCAN_PD_CENTER and SCAN_PD_FRAMES.  Calls READ_BIS from APEX.  Also 
;		calls ARREQ, ISNUM, RANGE_COMP, RIMG_MM, STREQ, STRPARSE_MM and TYPE, 
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 5-NOV-2008 by Mati Meron, as a stripped down and streamlined
;		version of SCAN_PD_READ.
;		Modified 25-FEB-2009 by Mati Meron.  Internal changes.
;		Modified 10-AUG-2009 by Mati Meron.  Internal changes.
;		Modified 5-FEB-2010 by Mati Meron.  Added keyword VERIFY.
;		Modified 25-MAR-2010 by Mati Meron.  Internal changes for APEX support.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 30-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 5-APR-2010 by Mati Meron.  Added keyword NFACT.
;		Modified 10-JUN-2010 by Mati Meron.  Added keyword GRID_CORR and its
;		corresponding common block, PGRID_CORR.
;		Modified 5-FEB-2011 by Mati Meron.  Internal changes.
;		Modified 5-APR-2011 by Mati Meron.  Added keyword FOFF.
;		Modified 10-OCT-2011 by Mati Meron.  Internal changes to accomodate 
;		camera data.
;		Modified 25-OCT-2011 by Mati Meron.  Added keyword ROTATE.
;		Modified 20-DEC-2011 by Mati Meron.  Added keyword READ_NEW and common 
;		block PREV_READ and, with it, the ability to use previously read data.
;		Modified 25-JUL-2015 by Mati Meron.  Internal changes.
;		Modified 25-AUG-2016 by Mati Meron.  Internal changes, Pilatus1M related
;		Modified 25-AUG-2016 by Mati Meron.  Internal changes, Pilatus1M related
;		Modified 5-OCT-2016 by Mati Meron.  Internal changes.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	common pgrid_corr, cval, carr
	common prev_read, lsnum, ldim, lwfnum, lres
	on_error, 1

	cent = Scan_PD_center( $
	snum,/sing,ori=ori,out=otm,dim=dim,rot=rot,stat=sta,_extra=_e)

	if sta then begin
		cur = fildat.scan[snum]
		wfnum= Scan_PD_frames(snum,fnum,verify=ver,nframes=nfnum,/uni,_extra=_e)
		if keyword_set(nrm) then begin
			ncol = ''
			if Type(nrm) eq 7 then ncol= nrm else if nrm eq 1 then ncol= 'monc'
			if ncol ne '' then begin
				col = Scan_column(snum,ncol)
				if Streq(ncol,'monc') and min(col) eq 0 then begin
					acol = Scan_column(snum,'amonc',sta=csta)
					if csta then col = acol
				endif
				if keyword_set(glb) then wco = 1./col[wfnum] $
				else wco = total(col)/cur.ncr[1]/col[wfnum]
			endif else wco = replicate(1.,nfnum)
		endif else wco = replicate(1.,nfnum)

		dum = Strparse_mm(cur.pdfnam,'_',lis)
		zlen = strlen(lis[dum])
		fcod = strcompress('(I0'+string(zlen)+')',/rem)
		gnam = fildat.pdpath + strjoin(lis[0:dum-1],'_') + '_'
		rnam = gnam + string(wfnum,fcod) + cur.pdfext
		tit= gnam + '(' + Range_comp(wfnum) +')'
		apfl = (cur.pdstat - 3)/2
		if apfl eq 1 then dim = dim < itop

		if not keyword_set(rnw) then begin
			if Isnum(lsnum) then begin
				if lsnum eq snum and Arreq(ldim,dim) and Arreq(lwfnum,wfnum) $
				then begin
					res= lres
					rfl = 0
				endif else rfl = 1
			endif else rfl = 1
		endif else rfl = 1

		if rfl then begin
			res = fltarr([nfnum,dim])
			for i = 0l, nfnum-1 do begin
				case apfl of
					0	:	dat = rotate(read_tiff(rnam[i]),rot)
					1	:	dat = Read_BIS(rnam[i],rot,itp=itop,_extra=_e)
					2	:	dat = Rimg_mm(rnam[i],rot=rot)
					3	:	message, 'Unknown!'
					4	:	dat = rotate(read_tiff(rnam[i]),rot)
				endcase
				res[i,*,*] = dat > 0
			endfor
			lsnum = snum
			ldim = dim
			lwfnum = wfnum
			lres = res
		endif

		for i = 0l, nfnum-1 do begin
			if keyword_set(bad) then res[i,*,*] = $
			Img_exorcise(reform(res[i,*,*]),(2.+2*bad)/bad,iter=bad,_extra=_e)
			res[i,*,*] = wco[i]*res[i,*,*]
		endfor
	endif else message, 'Bad or missing scan number!'

	if keyword_set(grc) and apfl eq 0 then begin
		adim = [195,487]
		scheck = Arreq(dim,adim) + 2*Arreq(dim,reverse(adim))
		if scheck gt 0 then begin
			if not Isnum(cval) then begin
				cval = 0.
				carr = make_array(dim,val=1.)
			endif
			grow = [ 59, 60, 61,  120,121,122,  181,182,183,  242,243,244, $
					303,304,305,  364,365,366,  425,426,427]
			gcol = [ 96, 97, 98]
			if scheck eq 2 then begin
				tem = grow
				grow = gcol
				gcol = tem
			endif
			grfl = 1
			if grc ne 1 then begin
				if grc ne cval then begin
					if abs(grc) lt 1 then begin 
						cval = grc
						carr[gcol,*] = 1. + cval
						carr[*,grow] = 1. + cval
					endif else begin
						message, 'Grid correction value too large', /con
						grfl = 0
					endelse
				endif
			endif
			if grfl and cval ne 0 then $ 
			for i = 0, nfnum-1 do res[i,*,*] = res[i,*,*]/carr
		endif else message, 'Unrecognized size, aborting grid correction', /con
	endif

	roifl = 0
	if n_elements(hroi) eq 2 then begin
		roifl = 1
		whroi = 0 > hroi < (dim[0]-1)
		whroi = [min(whroi,max=max),max]
	endif else whroi = [0,dim[0]-1]
	if n_elements(vroi) eq 2 then begin
		roifl = 1
		wvroi = 0 > vroi < (dim[1]-1)
		wvroi = [min(wvroi,max=max),max]
	endif else wvroi = [0,dim[1]-1]

	if roifl then begin
		if Isnum(fof) then begin
			nfof = n_elements(fof)
			if (nfof ge 2*nfnum) and (nfof mod 2 eq 0) then begin
				wfof = (reform(fof,2,nfof/2))[*,lindgen(nfnum)]
				omin = min(wfof[0,*], max=omax)
				if (whroi[0] + omin) lt 0 or (whroi[1] + omax) ge dim[0] $
				then message, 'Excessive horizontal offsets!'
				omin = min(wfof[1,*], max=omax)
				if (wvroi[0] + omin) lt 0 or (wvroi[1] + omax) ge dim[1] $
				then message, 'Excessive vertical offsets!'
			endif else message, 'Offsets - Frames numbers mismatch!'
			ores = res
			res = fltarr([nfnum,whroi[1]-whroi[0]+1,wvroi[1]-wvroi[0]+1])
			for i = 0l, nfnum-1 do begin
				ihroi = whroi + wfof[0,i]
				ivroi = wvroi + wfof[1,i]
				res[i,*,*] = ores[i,ihroi[0]:ihroi[1],ivroi[0]:ivroi[1]]
			endfor
		endif else res = res[*,whroi[0]:whroi[1],wvroi[0]:wvroi[1]]
	endif

	if arg_present(cor) then begin
		cor = fltarr([2,dim])
		cor[0,*,*] = (whroi[0] + findgen(dim[0]))#replicate(1.,dim[1])
		cor[1,*,*] = replicate(1.,dim[0])#(wvroi[0] + findgen(dim[1]))
	endif

	return, res
end