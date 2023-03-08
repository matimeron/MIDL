Pro CRL_transfeq, fene= fen, floc= flc, flen= fln, sene= sen, sloc= slc, slen=sln,$
	get = get, elem = ele, radius = rad, show = sho

	on_error, 1

	posib = ['Be','Al','C']
	alpesq = [6.81e-4, 1.08e-3, 1.46e-3]
	rad = Default(rad,0.2,/dtyp)
	ele = Default(ele,'Al',/dtyp)
	whi = Strmatch_mm(ele,posib)
	if whi ge 0 then begin
		con = 1e-3*rad/alpesq[whi]
		if How_many(fir=fen,sec=flc,thi=fln) eq 3 then begin
			msk = [1,1,1]
			if Type(get) eq 7 then begin
				gposib = ['energy','location','lenses']
				wha = Strmatch_mm(get,gposib,2)
				if wha ge 0 then msk[wha] = 0 else message, $
				'Allowed inputs are "Energy", "Location" and "Lenses" only!'
			endif
			if How_many(fir=sen,sec=slc,thi=sln,mas=msk,whi=opt) eq 2 then begin
				case opt of
					3	:	sln = con*sen^2*(fln/(con*fen^2) -1./flc + 1./slc)
					5	:	slc = 1./(1./flc - 1./con*(fln/fen^2 - sln/sen^2))
					6	:	sen = fen*sqrt(sln/(fln- con*fen^2*(1./flc-1./slc)))
					else:	message, 'Illegal S-input!'
				endcase
			endif else message, 'Two and only two S-variables allowed!'
		endif else message, 'ALL three of the F-variables are required!'
	endif else message, 'Lens material not in list!'

	if keyword_set(sho) then begin
		print
		print, fen, form = '("	First Energy	-	",f7.3," keV")'
		print, flc, form = '("	First Location	-	",f7.3, "m")'
		print, fln, form = '("	First Lens #	-	",f7.3)'
		print
		print, sen, form = '("	Second Energy	-	",f7.3," keV")'
		print, slc, form = '("	Second Location	-	",f7.3, "m")'
		print, sln, form = '("	Second Lens #	-	",f7.3)'
	endif

	return
end