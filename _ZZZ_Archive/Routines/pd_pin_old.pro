Pro PD_pin_old, sp_0, sp_1, sp_2, sp_3, sp_4, sp_5, sp_6, sp_7, frame = fnum, $
	verify = ver, hroi = hroi, vroi = vroi, raw= raw, angles= ang, signed= sgn,$
	relaxed = rel, sin_norm = snr, skew = skw, xy_region= xyr, z_region= zr, $
	limits = lim, auto_lim = aul, title = tit, log = log, min = min, max = max,$
	radius = rad, rthick = rth, rline = rli, rcolor = rco, grid = gri, $
	zbin = zbin, offset= off, progress= prg, tiny= tin, last= lst, wnew= new, $
	result = res, xy_result = xy_res, z_result = z_res, _extra = _e

;+
; NAME:
;		PD_PIN
; VERSION:
;		8.42
; PURPOSE:
;		Readout and display of PD pinhole type data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		PD_PIN, SP_0,... [,keywords]
; INPUTS:
;	SP_0, SP_1, SP_2, SP_3, SP_4, SP_5, SP_6, SP_7
;		List(s) of scan numbers, in any form recognizable by RANGE_PROC.  If
;		more than one scan is provided, patching in the horizontal and
;		vertical directions will be attempted (not fully implemented yet).
;
;		Alternatively, SP_O may already contain PD data in the form returned by
;		PD_PIN.  In such case it is used as is and additional inputs (if
;		any) are ignored.  Note that in this case Q-coordinates are assumed as
;		a default.  This can be changed using /RAW or /ANGLES.
;
;		Note:	In the second case PD_PIN has no way to verify that the data
;				provided is indeed pinhole data, not some other 2D data.  It
;				is up to the user to make sure that the right data is used.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	FRAME
;		Accepts a frame number or list of frame numbers, in any form
;		recognizable by RANGE_PROC.  Valid only when a single scan is used,
;		automatically disabled for multiple scans.
;		Note that the default operation is always to sum all frames.
;	/VERIFY
;		Switch.  If set, the existence of all frames to be processed is
;		verified and bad or missing frames are rejected.
;	HROI
;		Two element vector defining horizontal region of interest, in *pixels*.
;	VROI
;		Two element vector defining vertical region of interest, in *pixels*.
;	/RAW
;		Switch.  If set, the coordinates of the image are "frame #"	|
;		forhorizontal and "pixel #" for vertical, instead of Q_xy	|
;		and Q_z.  Note that if multiple scans are patched, the frame|	Only one
;		number listed does not correspond to a frame number in any	|	of these
;		specific scan.												|	two may
;	/ANGLES															|	be used.
;		Switch.  If set the coordinates of the image are the angles	|
;		DTHETA (for horizontal) and BETA for vertical, instead of	|
;		Q_xy and Q_z.												|
;	/SIGNED
;		Switch.  If set, Q_xy is multiplied by the sign of the DTH angle.
;		/SIGNED has no effect when /ANGLES is set.
;	/RELAXED
;		Switch.  If set, the internal transformation from angles to Q-values is
;		unconstrained.  By default it is constrained to the internal data
;		region, to avoid edge inaccuracies.
;	/SIN_NORM
;		Disabled on 5/11/09, as not needed.
;	SKEW
;		Numeric scalar, if given the data is skewed along the horizontal 
;		dimension.  The value of SKEW is proportional to the tangent of the 
;		skew angle.  Values larger than 1 (in abs. value) should not be used.
;
;		Note:	The eight keywords above are relevant to data readout.  In the
;				case where SP_0 contains pre-read data, these keywords are
;				ignored.
;	XY_REG
;		A two element vector specifying xy region of interest in [min,max]
;		order.  If given, only data within the range will be displayed.
;	Z_REG
;		Same as XY_REG, for z.
;
;		Note:	XY and Z are defined in terms of the coordinates used, whether
;				RAW, ANGLES or QVAL.
;		Note:	XY_REG and Z_REG can be both specified, to define a rectangular
;				region of interest.
;		Note:	If either XY_reg or Z_REG is provided as a single value, this
;				value is taken as the bottom of the range.
;	/LIMITS
;		Switch.  If set, the limits of the observable region, as defined by the
;		beam's footprint and the slits, are superimposed on the image.  Active
;		only when a single frame is displayed.
;	AUTO_LIM
;		When given as a non-zero scalar (all inputs are projected into the range
;		[0,1], specifies that only the data within the fraction AUTO_LIM of the
;		observable range (see LIMITS above) is to be read.  Specifically, 
;		setting /AUTO_LIM specifies that the whole observable region is to be
;		read.  Setting AUTO_LIM explicitly to 0 disables it, meaning the whole
;		frame is read.
;		
;		Note: If HROI (see above) is explicitly provided, AUTO_LIM is ignored.
;	TITLE
;		Character input to be used as the title of the plot and (optionally)
;		saved image file.
;	/LOG
;		Switch.  Specifies logarithmic display.
;	MIN
;		A scalar entry in the range (0,1) specifying minimal relative value to
;		be displayed.  The minimum is set at MIN*maximum(IMG) and all image
;		values less than the minimum are set to the minimum.
;	MAX
;		Same as minimum, for the maximal value to be displayed.  All image
;		values greater than the maximum are set to the maximum.
;	RADIUS
;		Value, in Angstrem, of a scattering sphere's radius.  If provided and
;		non-zero, a set of curves  representing the minima of the scattering
;		spherical form factor are superimposed on the displayed image.
;	RTHICK
;		Integer value, specifies the thickness of the form factor curves.
;		Default is 1
;	RLINE
;		Integer value, specifies the linestyle of the form factor curves.
;		Default is 0 (solid).
;	RCOLOR
;		A True Color value, specifies the color or the form factor curves.  
;		Default is white.
;	/GRID
;		Switch.  If set, a grid is displayed on top of the image.
;	ZBIN
;		Specifies bin size for the embedded routine PD_ZBIN (see there).  Any
;		value, not just power of 2, may be used.  Default value is 64 (in terms
;		of original, pre-binned channels.  Check out PD_ZBIN for more details.
;	OFFSET
;		Scalar value, specifies the OFFSET parameter for PD_ZBIN.  See there.
;	/PROGRESS
;		Switch.  If set, the evaluation progress is printed to the screen.
;	/TINY
;		Switch.  If set, the display window is reduced in size, making it
;		aappropriate for small laptops.
;	/LAST
;		Switch.  If set, the last processed data is reused.  User still has the
;		option to change regions, smoothing, linear-log display etc.
;	/WNEW
;		Switch.  If set, a new graphics window is created instead of the old
;		one being overwritten.  The windows at the disposal of PD_PIN are 28-31,
;		when the last one is reached the sequence reverts to the origin.
;	RESULT
;		Optional output, see below.
;	XY_RESULT
;		Optional output, see below.
;	Z_RESULT
;		Optional output, see below.
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
;		Note:	/NORM is set by default in PD_PIN, it can be disabled by
;				using NORM = 0 in the function call.
;
;		Note:	If any of the keywords /PNG, /JPG or /BMP is invoked in call
;				to PD_PIN, whether directly or through _EXTRA, the
;				appropriate image file will be generated by WIMG_MM.
; OUTPUTS:
;		None other than the graphic output and the opt. outputs.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the processed data, (cut down to region of interest only)
;	XY_RESULT
;		Returns the integrated (over Q_xy) data.
;	Z_RESULT
;		Returns the integrated (over Q_z) data.
; COMMON BLOCKS:
;		PD_PIN_KEEP.  Contains data pertaining to the last processed PD_PIN
;		data, as follows:
;
;		EXS		-	Flag, value of 1 indicates that the common block is defined.
;		FTYP	-	Last data type, RAW (0), ANGLES (1) or QVALS (2).
;		LRES	-	Last Result, i.e. data array.
;		LTIT	-	Last title.
;		LUTIT	-	Last "units title", Counts for raw or NCounts for normalized
;		LWIN	-	Index of last window used.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		THe restrictions of SCAN_PD_READ.
; PROCEDURE:
;		Data readout, if needed, performed through SCAN_PD_READ.  Horizontal and
;		vertical patching, if needed, performed using IMG_JOIN.  Region
;		processing is straightforward, rest is done in embedded routines.
;		Calls PD_GID_XYINT, PD_GID_ZINT, PD_GID_ZBIN, PD_PIN_RANGE and OP_SFF.
;		Calls IMG_JOIN, IMG_SCALE, SCAN_FIELD_READ, SCAN_PD_FRAMES and 
;		SCAN_PD_READ from SPEC.  Also calls ARRPACK, CLEAN_NAME, DEFAULT, 
;		DISPLAY_MM, ISNUM, LABELS, LEXISORT, MAKE_GRID, ONE_OF, RANGE_COMP, 
;		RANGE_PROC, SORPURGE, WHERINSTRUCT and WIMG_MM from MIDL.
; MODIFICATION HISTORY:
;		CREATED 30-MAR-2008 by Mati Meron as a modification of PD_GID.
;		Modified 25-APR-2008 by Mati Meron.  Internal changes.
;		Modified 15-MAY-2008 by Mati Meron following changes in PD_GID, see
;		there for details.
;		Modified 20-JUN-2008 by Mati Meron.  Internal changes.
;		Modified 1-AUG-2008 by Mati Meron.  Added keyword LIMITS.
;		Modified 15-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 5-MAR-2009 by Mati Meron.  Added keyword SIN_NORM.
;		Modified 10-APR-2009 by Mati Meron.  Internal changes enabling PIN-GID
;		mode.  Added keyword PROGRESS.
;		Modified 5-NOV-2009 by Mati Meron.  Disabled keyword SIN_NORM.
;		Modified 5-FEB-2010 by Mati Meron.  Added keyword VERIFY.
;		Modified 15-APR-2010 by MAti Meron.  Added keyword SKEW.
;		Modified 10-OCT-2010 by Mati Meron.  Improved superposition of form 
;		factor curves.  Added keywords RTHICK, RLINE and RCOLOR.
;		Modified 30-OCT-2013 by Mati Meron.  Added keyword AUTO_LIM.
;		Modified 15-NOV-2013 by Mati Meron.  Internal changes.  Added keyword 
;		GRID.
;		Modified 5-JUN-2015 by Mati Meron.  Bug fix.
;-

	common pd_pin_keep, exs, ltyp, lres, ltit, lutit, lwin
	on_error, 1

	bwnum = 28l
	wpad = 64l
	bsiz = 32l
	xsi = [31, 22]*bsiz
	ysi = [30, 21]*bsiz
	pof = [[0.25, 9.5],[0.25, 8.5]]*bsiz
	prl = [8,7]*bsiz
	auz = [512l, 256l]

	tifl = keyword_set(tin)
	xsi = xsi[tifl]
	ysi = ysi[tifl]
	pof = pof[*,tifl]
	prl = prl[tifl]
	auz = auz[tifl]

	if keyword_set(lst) then begin
		if Isnum(exs) then begin
			typ = ltyp
			res = lres
			siz = size(res)
			tit = Default(tit,ltit,/dtyp)
			utit = lutit
		endif else message, 'There is no previous data!'
	endif else begin
		typ = 1 - One_of(ang,raw)
		if (Wherinstruct('norm',_e))[0] ge 0 then utit='Ncounts' $
		else utit='Counts'

		if (size(sp_0))[0] ne 3 then begin
			prfl = keyword_set(prg)
			skfl = (tkfl = Isnum(skw))
			snum = Range_proc(Arrpack($
				sp_0,sp_1,sp_2,sp_3,sp_4,sp_5,sp_6,sp_7),/uni)
			nsnum = n_elements(snum)
			if n_elements(fnum) eq 0 or nsnum gt 1 then fnum = -1 $
			else fnum = Range_proc(fnum)
			if fnum[0] eq -1 or n_elements(fnum) gt 1 then lim = 0
 			if Isnum(hroi) then waul = 0 $
			else waul = 0 > Default(aul,1.-keyword_set(lim),/dtyp) < 1
			angs = Scan_field_read(snum,'Angs')
			bet = angs[*,1]
			dth = angs[*,2]
			rbet = 1e-2*round(1e2*bet)
			rdth = 1e-2*round(1e2*dth)
			s = Lexisort(rbet,rdth)
			snum = snum[s]
			rbet = rbet[s]
			rdth = rdth[s]
			if waul ne 0 then begin
				sall = Scan_field_read(snum[0],'sl_val')
				sl5 = reform((reform(sall,n_elements(sall)/4,4))[5,2:3])
				swei = sl5*[1,-1]/total(sl5)
			endif
			ss = Sorpurge(rbet,net=ii)
			for i = 0l, ii-1 do begin
				ilis = where(rbet eq rbet[ss[i]],jj)
				for j = 0, jj-1 do begin
					if (Scan_field_read(snum[ilis[j]],'varan'))[2] then begin
						klis = Scan_PD_frames(snum[ilis[j]],fnum,verify=ver,$
							nframes=kk,_extra=_e)
						for k = 0, kk-1 do begin
							if prfl then $
							print, snum[ilis[j]], klis[k], form = '(i6,i4)'
							if i eq 0 and j eq 0 and k eq 0 and waul ne 0 then $
							begin
								pran = PD_pin_range(snum[ilis[0]],klis[0],/raw)
								del = (pran[1]-pran[0])*(1-waul)
								hroi = pran + round(del*swei)
							endif
							ttem = $
							Scan_PD_read(snum[ilis[j]],klis[k],ver=ver,/indiv,$
							hroi=hroi,vroi=vroi,/norm,raw=raw,angles=ang,$
							/pinhole,signed=sgn,relax=rel,title= dtit,_extra=_e)
							if i eq 0 and j eq 0 and k eq 0 and skfl then begin
								sdim = (size(ttem,/dim))[1:2]
								scal= (1 + skw*Make_grid([-1,1],sdim[0]))# $
								replicate(1.,sdim[1])
								tkfl = 0
							endif
							if skfl then ttem = Img_scale(ttem,scal)
							if k eq 0 then tem = ttem $
							else tem = Img_join(tem,ttem,/hor,_extra = _e)
						endfor
						if kk gt 1 then lim = 0
					endif else begin
						if prfl then print, snum[ilis[j]], form = '(i6,i4)'
						if i eq 0 and j eq 0 and waul ne 0 then begin
							pran = PD_pin_range(snum[ilis[0]],0,/raw)
							del = (pran[1]-pran[0])*(1-waul)
							hroi = pran + round(del*swei)
						endif
						tem = Scan_PD_read(snum[ilis[j]],fnum,ver=ver,$
						hroi=hroi,vroi=vroi,/norm,raw= raw,angles= ang,$
						/pinhole,signed=sgn,relaxed=rel,title= dtit,_extra= _e)
						if i eq 0 and j eq 0 and tkfl then begin
							sdim = (size(tem,/dim))[1:2]
							scal= (1 + skw*Make_grid([-1,1],sdim[0]))# $
							replicate(1.,sdim[1])
						endif
					endelse
					if tkfl then tem = Img_scale(tem,scal)
					if i eq 0 then begin
						if j eq 0 then res = tem $
						else res = Img_join(res,tem,/hor,_extra = _e)
					endif else begin
						if j eq 0 then tres = tem $
						else tres = Img_join(tres,tem,/hor,_extra = _e)
					endelse	
				endfor
				if i gt 0 then res = Img_join(res,tres,/ver,_extra=_e)
			endfor

			if keyword_set(lim) and nsnum eq 1 then begin
				hlim = PD_pin_range(snum,fnum[0],raw=raw,ang=ang,_extra=_e)
				vlim = [min(res[1,*,*],max=max),max]
				mark = transpose([[hlim],[vlim]])
			endif else mark = [0,0,0,0]

			if nsnum eq 1 then begin
				dtit = strmid(dtit,0,strpos(dtit,'_',/reverse_search))
				wfnum= Scan_PD_frames(snum,fnum,ver=ver,/uni,comp=cfl,_extra=_e)
				if not cfl then dtit = dtit + ' (' + Range_comp(wfnum)+ ')'
			endif else dtit = $
			strmid(dtit,0,strpos(dtit,'_')) +' S# ' + Range_comp(snum)
		endif else res = sp_0

		siz = size(res)
		if siz[0] ne 3 then message, 'Invalid Data!'
		tit = Default(tit,Default(dtit,''))

		exs = 1
		ltyp = typ
		lres = res
		ltit = tit
		lutit = utit
	endelse
	xytit = [['pix','Dth','Q!dxy!n'],['pix','Beta','Q!dz!n']]
	xtit = xytit[typ,0]
	ytit = xytit[typ,1]
	lfl = keyword_set(log)

	qxyran = [min(reform(res[0,*,0]),max=maxx),maxx]
	qzran = [min(reform(res[1,0,*]),max=maxx),maxx]

	case n_elements(xyr) of
		0	:	wxy = qxyran
		1	:	wxy = [xyr[0],qxyran[1]]
		2	:	wxy = 1.*xyr
		else:	wxy = 1.*xyr[0:1]
	endcase
	wxy[0] = wxy[0] > qxyran[0]
	wxy[1] = wxy[1] < qxyran[1]

	case n_elements(zr) of
		0	:	wz = qzran
		1	:	wz = [zr[0],qzran[1]]
		2	:	wz = 1.*zr
		else:	wz = 1.*zr[0:1]
	endcase
	wz[0] = wz[0] > qzran[0]
	wz[1] = wz[1] < qzran[1]

	dum= where(res[0,*,0] ge wxy[0] and res[0,*,0] le wxy[1],ndum)
	if ndum eq 0 then begin
		wxy = qxyran
		message, 'Bad XY range, ignoring!', /continue
	endif else res = res[*,dum,*]
	dum = where(res[1,0,*] ge wz[0] and res[1,0,*] le wz[1], ndum)
	if ndum eq 0 then begin
		wz = qzran
		message, 'Bad X range, ignoring!', /continue
	endif else res = res[*,*,dum]

	bin = ceil(1.*siz[2:3]/auz)

	if keyword_set(new) then begin
		lwin = Default(lwin,bwnum-1)
		wnum = bwnum + ((lwin - bwnum + 1) mod 4)
	endif else wnum = Default(lwin,bwnum)
	lwin = wnum

	Display_mm, res, auz= auz, bin= bin, wsi=wsi, /nodata
	xsi = wsi[0] + prl + 3*wpad/4
	ysi = wsi[1] + prl + 3*wpad/2
	window, wnum, xsiz = xsi, ysiz = ysi, $
	tit = strcompress('IDL ' + string(wnum) + ' : PD Pinhole Results')
	Display_mm, res, min= min, max= max, poff= pof, auz= auz, bin= bin, /ave, $
	tit= 'Raw Data', xtit = xtit, ytit = ytit, log = log, isi = isi, wsi = wsi,$
	mark = mark, grid = gri, ppos = pps, ran = ran, _extra = _e
	if Isnum(rad) and typ eq 2 then $
	Op_sff, rad, ppos = pps, ran = ran, thick = rth, line = rli, color = rco

	pos = [0,0,xsi-1,ysi-1]
	if typ eq 0 then form = '(i0)' else form = '(f7.3)'
	swxy = strcompress('[' + strjoin(string(wxy,form=form),',') + ']')
	swz  = strcompress('[' + strjoin(string(wz, form=form),',') + ']')
	itit = strjoin([xtit+ ' range = '+ swxy, ytit+ ' range = '+ swz],'  ;  ')
	Labels, xsi/2*[1,1], ysi-bsiz/8*[5,10], [tit,itit], align=0.5,chars=1.2,/dev

	pos = [xsi-prl-wpad,pof[1]+wpad,xsi-wpad-1,pof[1]+isi[1]+wpad-1]
	PD_GID_xyint, res, log = log, raw = raw, angles = ang, /bare, res = xy_res,$
	/flip, /noerase, title = 'Integrated over ' + xtit, position = pos, /dev, $
	xtit = utit, ytit = '', ytickname = replicate(' ',30)
	axis, yaxis = 1, ystyle = 1, ytit = ytit, _extra = _e

	pos = [pof[0]+wpad,3*wpad/4,pof[0]+isi[0]+wpad-1,prl+3*wpad/4-1]
	PD_GID_zint, res, log = log, raw = raw, angles = ang, /bare, res = z_res, $
	/noerase, xtit = xtit, ytit= utit, title ='Integrated over '+ ytit, $
	position = pos, /dev, _extra = _e

	pos = [xsi-prl-wpad,3*wpad/4,xsi-wpad-1,prl+3*wpad/4-1]
	PD_GID_zbin, res, zbin = zbin, off = off, log = log, raw= raw, angles= ang,$
	/zpos, /noerase, title= ytit + '-binned', xtit = xtit, ytit= 'a.u', $
	labco = !pcol.purple, position = pos, /dev

	wshow, wnum
	Wimg_mm, Clean_name(tit), call = 2, /nodef, /verb, _extra = _e

	return
end