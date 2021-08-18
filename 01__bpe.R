#!/usr/bin/env Rscript
#
# Byte-pair encoding. [Diorisis first]
#####################
library( 'tokenizers.bpe' )
library( 'ggplot2' )

# outdir
outdir <- 'out.01.bpe'
dir.create( outdir, showWarnings = FALSE )

# infile
infile <- "data/diorisis.txt"

# read lines [164,195]
length( df <- readr::read_lines( infile ) )

# replace all , and . with ' ,' en ' .'
#df <- gsub( ',', ' ,', df )
#df <- gsub( '\\.', ' \\.', df )

df <- gsub( '’’', ' ', df )

# remove double spaces
df <- gsub( '   ', ' ', df )
df <- gsub( '  ', ' ', df )

# get rid of replaced chars
df <- gsub( '__', '_', df )
df <- gsub( '__', '_', df )
df <- gsub( '__', '_', df )
df <- gsub( '__', '_', df )
df <- gsub( '__', '_', df )
df <- gsub( '__', '_', df )
df <- gsub( '__', '_', df )


# count words
words <- strsplit( df, " ", fixed = TRUE )
length( words <- unlist( words ) ) # 11,519,481
counts <- data.frame( table( words ) )

# get sorted counts
idx <- sort.int( counts$Freq, decreasing = TRUE, index.return = TRUE )
		  
# get frequencies from high to low
freqs <- counts[ idx$ix, ]
rownames( freqs ) <- NULL
colnames( freqs ) <- c( 'token', 'frequency' )

# clean frequencies
clean_freqs <- freqs[ !freqs$token %in% c( ',', '.', ';', '_', '·' ), ]

# get index of words with one or more underscores
idx_underscore <- stringr::str_detect( clean_freqs$token, '_' )

# 510,974 unique words
n_unique_words <- nrow( clean_freqs <- clean_freqs[ ! idx_underscore, ] )

# 10,158,162 all words
n_all_words <- sum( clean_freqs$frequency, na.rm = TRUE )

# write to disk
write.csv( clean_freqs, file = gzfile( paste0( outdir, '/clean_freqs.csv.gz' ) ) )


#############################
# BPE
#############################

all <- NULL

for( vocab_size in c( 1000, 1500, 2500, 5000, 7500, 10000, 12500, 15000, 20000, 30000 ) )
{
	print( vocab_size )
	model <- tokenizers.bpe::bpe( infile, coverage = 0.9999, vocab_size = vocab_size, 
							  model_path = paste0( outdir, '/bpe_model_', vocab_size, '.bin' ) )

	# get string lengths
	nchars <- nchar( model$vocabulary$subword )

	single <- data.frame( vocab_size, nchars )

	all <- rbind( all, single )
}

# plot density
all$size <- as.factor( all$vocab_size )
all$subset <- '<10,000'
all[ all$vocab_size >= 10000, 'subset' ] <- '>10,000'
all$subset <- as.factor( all$subset )

p <- ggplot( data = all, aes( x = nchars, colour = size, group = size ) ) + 
	geom_density( alpha = 0.1 ) +
	facet_wrap( ~subset, nrow = 2 ) + 
	scale_colour_brewer( palette = 'RdYlBu' ) +
	scale_fill_brewer( palette = 'RdYlBu' ) +
	xlab( "Number of characters" ) +
	ylab( "Density" ) + 
	scale_x_continuous( breaks = 1:14 ) +
	theme_light( base_family = 'Arno Pro' ) +
	theme( plot.title = element_text( color = "black", size = 10, face = "bold" ),
		   strip.text.x = element_text( size = 8, face = 'bold' ),
		   axis.text.x = element_text( size = 8, colour = 'gray30', angle = 0 ),
		   axis.text.y = element_text( size = 8, colour = 'gray30', angle = 0 ),
		   axis.title = element_text( size = 10, colour = 'gray30', face = 'bold' ) )
	
# save plot to disk
outfile <- paste0( outdir, '/bpe__vocabularies.png' )
ggsave( file = outfile, plot = p, height = 5, width = 5, dpi = 300 )

# conclusion: a vocabulary of 12,500 or 15,000 for diorisis is sufficient.

#text <- c( "Σιδὼν ἐπὶ θαλάττῃ πόλις , Ἀσσυρίων ἡ θάλασσα , μήτηρ Φοινίκω
#ν ἡ πόλις , Θηβαίων ὁ δῆμος πατήρ · δίδυμος λιμὴν ἐν κόλπῳ πλατύς , ἠρέμα κλεί
# ων τὸ πέλαγος . Ἧι γὰρ ὁ κόλπος κατὰ πλευρὰν ἐπὶ δεξιὰ κοιλαίνεται , στόμα δεύτερον ὀρώρυκται , κα ")
#bpe_encode(model, x = text, type = "subwords")

 