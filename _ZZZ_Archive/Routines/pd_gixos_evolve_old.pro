Pro PD_gixos_evolve_old, snum, fnum, bnum = bnum, bfnum = bfnm, verify = ver, $
	hroi = hroi, vroi = vroi, raw = raw, angle = ang, frames = fra, $
	t_reg = trg, z_reg = zrg, bin = bin, smooth = smo, $
	last = lst, wnew = wnw, plot = plt, wait = wai, $
	result = res, z_result = z_res, write = wri, _extra = _e

;+
; NAME:
;		PD_GIXOS_EVOLVE
; VERSION:
;		8.42
; PURPOSE:
;		Readout and display of PD GIXOS Data
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		PD_GIXOS_EVOLVE, SNUM, FNUM [ BNUM = BNUM, BFNUM = BFNUM] [ keywords]
; INPUTS:
;	SNUM
;		Single scan number.  The scan will be horizontally integrated, frame by
;		frame, to provide GIXOS data.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Optional list of frame numbers, in any form acceptable by RANGE_PROC.
;		By default, all the frames within SNUM are used.
;
;		Note:	If some of the frame numbers are out of the range present,
;		they're ignored.  If there are no frame numbers within the range
;		present, an error message is issued.
; KEYWORD PARAMETERS:
;	BNUM
;		Single scan number.  If given, the average of horizontally integrated
;		frames of BNUM will be used as GIXOS background data.
;	BFNUM
;		Optional list of frame numbers, in any form acceptable by RANGE_PROC.
;		By default, all the frames within BNUM are used.  Same rules as those
;		mentioned above, in the note to FNUM, are followed.
;
;		Note:	If some of the frame numbers are out of the range present,
;		they're ignored.  If there are no frame numbers within the range
;		present, an error message is issued.
;	/VERIFY
;		Switch.  If set, the existence of all frames to be processed is
;		verified and bad or missing frames are rejected.
;	HROI
;		Two element vector defining horizontal region of interest, in *pixels*.
;	VROI
;		Two element vector defining vertical region of interest, in *pixels*.
;	/RAW
;		Switch.  If set, the vertical coordinate of the image	|
;		is "pixel #", instead of Q_z.							|	only one of
;	/ANGLE														|	these two
;		Switch.  If set, the vertical coordinate of the image 	|	may be used
;		is BETA, instead of Q_z.								|
;	/FRAMES
;		Switch.  If set, the horizontal coordinate of the image is "frame #'.
;		The default is time in secondes (from the beginning of the scan.
;	T_REG
;		A two element vector specifying time (horizontal) region of interest in
;		[min,max] order.  If given, only data within the range will be
;		displayed.  Note that if FRAMES (see above) is set, T_REG refers to
;		frame numbers.
;	Z_REG
;		Same as T_REG, for z.  By default refers to Q_z region but this changes
;		if RAW or ANGLES (see above) is used.
;		Note:	T_REG and Z_REG can be both specified, to define a rectangular
;				region of interest.
;		Note:	If either T_reg or Z_REG is provided as a single value, this
;				value is taken as the bottom of the range.
;	BIN
;		Positive integer, specifies the bin size to be applied to the scan data,
;		in the horizontal direction.  The binned data will have horizontal 
;		dimension of FLOOR(nframes/bin), where nframes is the number of frames
;		in the scan.  The vertical dimension is not affected.
;	SMOOTH
;		Positive integer, specifying smoothing (binomial, by default) width (see
;		IMG_SMOOTH for details) in the vertical dimension only.  Should be an
;		odd number, if even, the next higher odd number will be used.
;	/LAST
;		Switch.  If set, the last processed data is reused.  User still has the
;		option to change coordinates, binning, regions, linear-log display etc.
;	/WNEW
;		Switch.  If set, a new graphics window is created instead of the old
;		one being overwritten.  The windows at the disposal of PD_GIXOS_EVOLVE
;		are 16-19, when the last one is reached the sequence reverts to the 
;		origin.
;	/PLOT
;		Switch.  When set, all the individual GIXOS scans in the data are
;		plotted together, in PD_VIEW style.
;	/WAIT
;		Switch.  If set in conjuction with PLOT, the plots are displayed one 
;		after another, waiting for the user to hit a key to continue.
;		Alternatively, if WAIT is given as negative value, the program waits
;		for the time specified by the absolute value of WAIT before displaying
;		the next plot.
;	RESULT
;		Optional output, see below.
;	Z_RESULT
;		Optional output, see below.
;	/WRITE
;		Switch.  If set, the data is unpacked into individual GIXOS scans and
;		each scan is saved into a separate file.  The routine will interactively
;		query for an output folder, in this case. 
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  This includes (among others) the following keywords,
;		affecting data readout:
;
;			/BAD
;				Switch.  If set, faulty (very high count rate) pixels are
;				removed from the data.
;			/NORM
;				Switch.  If set, the data is normalized to monitor counts.
;
;		See SCAN_PD_READ for more details.
;
;		Note:	/NORM is set by default in PD_GIXOS_EVOLVE.  It can be disabled
;				by using NORM = 0 in the function call.
;
;		Note:	If any of the keywords /PNG, /JPG or /BMP is invoked in call
;				to PD_GID, whether directly or through _EXTRA, the
;				appropriate image file will be generated by WIMG_MM.
;
;		Note:	All the DISPLAY_MM keywords and SCAN_SHOW keywords are available
;				as well.
; OUTPUTS:
;		None other than the graphic output and the opt. outputs.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the processed data.  If regions of interest are defined, this
;		will be reflected in RESULT.
;	Z_RESULT
;		Returns the integrated (over Q_z) data.
; COMMON BLOCKS:
;		PD_GIXOS_EVOLVE_KEEP.  Contains data pertaining to the last processed 
;		PD_GID_EVOLVE data, as follows:
;
;		GEXS	-	Flag, value of 1 indicates that the common block is defined.
;		WSNUM	-	The number of the last signal scan processed.
;		LRES	-	Last result, i.e. data array.
;		FRM		-	List of frame numbers for the last signal scan processed.
;		TIM		-	List of times (in seconds) for the last signal scan 
;					processed.
;		TIT		-	The title for the last data processed, 2-element character
;					array, including both signal and background information.
;		WNUM	-	Index of last window used.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Reads and horizontally integrates, frame by frame, the signal data, 
;		then, if needed, subtracts the average of horizontally integrated
;		background data.  
;		Calls IMG_INT, IMG_SMOOTH, SCAN_PD_COMB, SCAN_PD_LCOO, SCAN_PD_READ,
;		SCAN_SHOW and SPEC_FILE_CHECK.  Calls WRITE_DATA from Surface.  Calls
;		ARRLOC, CLEAN_NAME, DEFAULT, DISPLAY_MM, FILE_GET, FNAMPARSE, ISNUM,
;		LABELS, ONE_OF, SM_ERMULT, STREQ, STRPARSE_MM, WHERINSTRUCT and WIMG_MM,
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 5-JAN-2014 by Mati Meron.
;		Modified 10-JAN-2014 by Mati Meron.  Added keyword BIN.
;		Modified 5-OCT-2014 by Mati Meron.  Added keyword WRITE.
;		Modified 5-JUN-2015 by Mati Meron.  Added keywords PLOT and WAIT.
;		Modified 5-JUL-2015 by MAti Meron.  Added keyword SMOOTH.
;-

	common pd_gixos_evolve_keep, gexs, wsnum, lres, frm, tim, tit, wnum
	on_error, 1

	bwnum = 16l
	bsiz = 32l
	wpad = 64l
	htit = ['frame #', 'time (seconds)']
	vtit = ['pix.','Beta','Q!dz!n']

	if keyword_set(lst) then begin
		if not Isnum(gexs) then message, 'There is no previous data!'
		siz = size(lres)
	endif else begin
		Spec_file_check, snum, /sing, /pildet, /close, list = wsnum
		lres = Scan_PD_comb(wsnum,fnum,hroi=hroi,vroi=vroi,verify=ver,$
		/norm,/bad,/reg,/other,frame=frm,time=tim,tit=stit,_extra=_e)
		siz = size(lres)
		if keyword_set(bnum) then begin
			Spec_file_check, bnum, /sing, /pildet, /close, list = wbnum
			bac = Scan_PD_read(wbnum,bfnm,hroi=hroi,vroi=vroi,verify=ver,$
			/raw,/per,/norm,/bad,tit=btit,_extra=_e)
			ibac = Img_int(bac,/xy_int)
			lbac = lres
			lbac[2,*,*] = replicate(1,siz[2])#reform(ibac[1,*])
			lbac[3,*,*] = replicate(1,siz[2])#reform(ibac[2,*])
			lres[2,*,*] = (lres[2,*,*] - lbac[2,*,*]) > 0
			lres[3,*,*] = sqrt(lres[3,*,*]^2 + lbac[3,*,*]^2)
			dum = Strparse_mm(btit,'_',lis)
			tem = lis[-1]
			strput, tem, 'AVE'
			lis[-1] = tem
			tit = [stit,'Background:  ' + strjoin(lis[1:-1],'_')]
		endif else tit = [stit,'No background']
		gexs = 1
	endelse

	dtyp = 1 - [keyword_set(fra),One_of(ang,raw)]
	if dtyp[0] then xy = tim else xy = frm
	if dtyp[1] eq 0 then begin
		zz = lindgen(siz[3])
		if keyword_set(vroi) then zz = zz + vroi[0]
	endif else zz = Scan_PD_lcoo(wsnum,dir='ver',ang=ang,vroi=vroi)
	lres[0,*,*] = xy#replicate(1.,siz[3])
	lres[1,*,*] = replicate(1.,siz[2])#zz
	res = lres

	wbin = Default(bin,1,/dtyp) > 1
	if wbin gt 1 then begin
		blen = siz[2]/wbin
		ores = res[*,0:wbin*blen-1,*]
		res = fltarr(4,blen,siz[3])
		ind = (lindgen(blen)+1)*wbin - 1
		res[0:1,*,*] = ores[0:1,ind,*]
		res[2,*,*] = wbin*rebin(reform(ores[2,*,*]),blen,siz[3])
		res[3,*,*] = sqrt(wbin*rebin(reform(ores[3,*,*])^2,blen,siz[3]))
	endif

	if Isnum(smo) then begin
		res = Img_smooth(res,smo,/yonly, ker=ker,_extra=_e)
		erm = SM_ermult(ker)
	endif else erm = [1.,1.]

	if n_elements(trg) gt 0 then begin
		tval = reform(res[0,*,0])
		wtrg = ([trg[sort(trg)],max(tval)])[0:1]
		dum = where(tval ge wtrg[0] and tval le wtrg[1])
		res = res[*,dum,*]
	endif

	if n_elements(zrg) gt 0 then begin
		zval = reform(res[1,0,*])
		wzrg = ([zrg[sort(zrg)],max(zval)])[0:1]
		dum = where(zval ge wzrg[0] and zval le wzrg[1])
		res = res[*,*,dum]
	endif

	Display_mm, res, /auz, wsi=wsi, /nodata
	wsi = (wsi+1)/2*2
	xsi = wsi[0] + bsiz
	ysi = wsi[1] + 11*bsiz
	pof = bsiz*[0.5,9.5]
	if keyword_set(wnw) $
	then wnum = bwnum + ((Default(wnum,bwnum-1) - bwnum + 1) mod 4) $
	else wnum = Default(wnum,bwnum)
	window, wnum, xsiz = xsi, ysiz = ysi, $
	tit = strcompress('IDL ' + string(wnum) + ' : PD GIXOS EVOLVE Results')
	Display_mm, res, /auz, poff= pof, xtit=htit[dtyp[0]], ytit=vtit[dtyp[1]], $
	isi = isi, _extra = _e

	Labels, xsi*[0.5,0.5], ysi-bsiz*[0.75,1.5], [tit], align=0.5,chars=1.2,/dev

	pos = [pof[0]+wpad,1.5*bsiz,pof[0]+isi[0]+wpad-1,9.5*bsiz-1]
	z_res = Img_int(res,/z_int)
	z_res[2,*,*] = erm[0]*z_res[2,*,*]
	Scan_show, z_res, pos= pos, /dev, xstyle= 1, /noerase, /nofile, $
	tit= 'Integrated over '+ vtit[dtyp[1]], xtit=htit[dtyp[0]], ytit='Counts',$
	_extra=_e

	Wimg_mm, Clean_name(tit[0]), call = 2, /nodef, /verb, _extra = _e

	if keyword_set(plt) then begin
		window, (wnum - bwnum + 2) mod 4
		nrain = n_elements(!rainbow)
		ylfl = ((Wherinstruct('ylo',_e))[0] ge 0)
		wai = Default(wai,0.,/dtyp)
		pres = res[1:3,*,*]
		hco = reform(pres[0,0,*])
		tem = reform(pres[1,*,*])
		siz = size(tem)
		max = max(tem,loc)
		ihi = (Arrloc(loc,siz))[0]
		plot, hco, tem[ihi,*]+ylfl, /nodata, ymar=[4,4],tit=tit[0]+'!c'+tit[1],$
			xtit= vtit[dtyp[1]], ytit= 'Counts',_extra = _e
		if wai gt 0 then print, string([13b,9b,9b]) + $
			'Hit "Q" to exit, any other key to continue'
		for i= 0, siz[1]-1 do begin
			oplot, hco, tem[i,*] + ylfl, col = !rainbow[i mod nrain]
			if wai gt 0 then begin
				dum = (dum = get_kbrd())
				if Streq(dum,'q',1) then break
			endif else if wai lt 0 then wait, abs(wai)
		endfor
	endif

	if keyword_set(wri) then begin
		nam = Fnamparse(tit[0])
		dum = Strparse_mm(nam,'_',lis)
		pnam = strjoin(lis[0:dum-1],'_')
		dim = size(res,/dim)
		odir = File_get(/dir,title='Select output folder',_extra=_e)
		for i = 0, dim[1] -1 do begin
			snam = pnam + string(long(res[0,i,0]),form='("_",i0,"s.txt")')
			Write_data, reform(res[1:3,i,*]), snam, /auto
		endfor
	endif

	return
end