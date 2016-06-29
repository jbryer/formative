txt <- ''

# TODO: revisit the cut points. 
if(daacs.scored[daacs.scored$Factor == i,]$percentile > 0.66) { # High
	lvl <- 'High'
	txt <- sasr.feedback[sasr.feedback$Factor == i,]$High
} else if(daacs.scored[daacs.scored$Factor == i,]$percentile > 0.33) { # Medium
	lvl <- 'Medium'
	txt <- sasr.feedback[sasr.feedback$Factor == i,]$Medium
} else { # Low
	lvl <- 'Low'
	txt <- sasr.feedback[sasr.feedback$Factor == i,]$Low
}

cat(paste0('You scored **', tolower(lvl), '** on the ', i, ' domain, scoring at the ',
		   formatPercentile(daacs.scored[daacs.scored$Factor == i,]$percentile), ' percentile.\n\n'))
cat('\n\nThe items that contribute to this domain are:\n\n')

items <- sasr34[sasr34$Subscale == i,]
scores <- as.integer(student.daacs[1, sasr.pos[which(sasr34$Subscale == i)]])

tmp <- data.frame(Item=items$SASR.34.Item, Response=(7-scores), stringsAsFactors=FALSE)
tmp$xend <- 0
tmp$reverse <- items$Reverse.Score == 'Yes'
if(sum(tmp$reverse) > 0) {
	tmp[tmp$reverse,]$xend <- 7
	tmp[tmp$reverse,]$Item <- paste0(tmp[tmp$reverse,]$Item, '\n(reverse scored)')
}
tmp$value <- abs(tmp$Response - tmp$xend)
tmp$hjust <- ifelse(tmp$reverse, 3, -2)

print(ggplot(tmp, aes(x=Response, y=Item, color=value)) +
	  	geom_segment(aes(xend=xend, yend=Item)) +
	  	geom_point(size=5) +
	  	geom_text(aes(label=value, hjust=hjust), size=4) +
	  	theme(axis.ticks.x=element_blank(), legend.position='none') +
	  	ylab('') + xlab('') +
	  	scale_x_continuous(limits=c(0,7), breaks=1:5, labels=rev(gsub(' ', '\n', sasr.levels))) + 
	  	scale_color_continuous(limits=c(1,6), low='#FF9900', high='#660066'))

# Specific feedback
cat(txt, '\n\n')
