Pro AD_roi, snum, hroi = hroi, vroi = vroi, reset = rst, immed = imm, $
	bad = bad, show = sho, test = tst, _extra = _e

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	common ad_roi_keep, pexs, path, num, lfile

	on_error, 1
	
	ext = '.sfrm'
	sfexs = Default(exs,0,/dtyp)

	if not keyword_set(rst) then begin
		pexs = Default(pexs,0,/dtyp)
		num = Default(num,0,/dtyp)
	endif else pexs = (num = 0)

	if not pexs then begin
		if Isnum(snum) and sfexs then begin
			path = fildat.pdpath
			sta = 1
		endif else path = File_get(/dir,sta=sta)
		if sta then pexs = 1
		cofl = 0
	endif else cofl = 1
	if not pexs then message, 'Path not defined, exiting!'

	badfl = keyword_set(bad)
	shofl = keyword_set(sho)
	tstfl = keyword_set(tst)

	done = 0
	repeat begin
		ffile = File_last(path=path,ext=ext,num=nnum)
		if nnum gt num then begin
			lfile = ffile
			if cofl then begin
				if (nnum - num) gt 1 then $
				message, 'Lost count, skipped files!',/con
				gofl = 1
			endif else gofl = keyword_set(imm)
			num = nnum
		endif else gofl = 0
		cofl = 1

		if gofl then begin
			dat = Read_BIS(lfile,dim=isiz,_extra=_e)
			whroi = 0 > Default(hroi,[0l,isiz[0]-1],/dtyp) < (isiz[0]-1)
			wvroi = 0 > Default(vroi,[0l,isiz[1]-1],/dtyp) < (isiz[1]-1)
			mark = [whroi[0],wvroi[0],whroi[1],wvroi[1]]
			bin = isiz/512 > 1
			tit = Fnamparse(lfile)
			if badfl then dat = Img_exorcise(dat,2,iter=bad)
			if shofl then Display_mm, dat,tit=tit,bin=bin,mark=mark,_extra=_e
			roidat = dat[whroi[0]:whroi[1],wvroi[0]:wvroi[1]]
			troidat= total(roidat,/double)
			if not tstfl then begin
				t = caput('APEX2:ROI1TotalCounts.VAL',troidat)
				t = caput('APEX2:AcquireDone.VAL',0)
			endif
			print, tit, '	:	', troidat, max(dat), max(roidat)
		endif

		dum = (dum = get_kbrd(0))
		if Streq(dum,'q',1) then done = 1
	endrep until done
	print, 'Done'
		
	return
end				