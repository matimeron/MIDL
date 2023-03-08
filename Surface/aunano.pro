Function Aunano, corsize, fulsize, pack_frac = paf, step = stp, $
	edensity = eld, tanphi = tph,  show = sho

	on_error, 1

	zval = Nano_eld(corsize,fulsize,pack=paf,step=stp,core='Au',liga='C12H26S',$
		dlig=0.845,edensity=eld,tanphi=tph,show=sho)

	return, zval
end