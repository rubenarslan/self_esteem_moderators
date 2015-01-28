library(shiny)
library(dplyr)
library(ggplot2)

countries = c("Canada", "United States", "Australia", "New Zealand", "Argentina", 
							"Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Dominican Republic", 
							"Ecuador", "Guatemala", "Mexico", "Peru", "Venezuela", "Denmark", 
							"Finland", "Ireland", "Norway", "Sweden", "United Kingdom", "Austria", 
							"Belgium", "France", "Germany", "Netherlands", "Switzerland", 
							"Greece", "Italy", "Portugal", "Romania", "Spain", "Egypt", "South Africa", 
							"China", "Hong Kong", "India", "Japan", "Korea, Republic of", 
							"Philippines", "Indonesia", "Malaysia", "Singapore", "Thailand", 
							"Israel", "Turkey", "United Arab Emirates")

get_fitted = function(spag_plot_data, mplusfits, mod) {
	zmod = paste0("z",mod)
	spag_plot_data$mod = spag_plot_data[, zmod]
	spag_plot_data$E_sex = -1.85
	spag_plot_data$E_age = 0.80
	spag_plot_data$E_sex_age = -0.03
	spag_plot_data$E_mod_age = mplusfits[mplusfits$indicator_short == mod,]$Age
	spag_plot_data$E_mod_sex = mplusfits[mplusfits$indicator_short == mod,]$Gender
	spag_plot_data$E_mod_sex_age = mplusfits[mplusfits$indicator_short == mod,]$Age_Gender
	
	spag_plot_data = within(spag_plot_data, { 
		mod_main = 49.6 + 
		sex * E_sex +
		agebin * E_age + 
		sex * agebin * E_sex_age
	})
	spag_plot_data = within(spag_plot_data, { 
		mod_fitted_simple_intercept = 49.6 + 
		sex * E_sex + 
		agebin * E_age + 
		agebin * sex * E_sex_age + 
		agebin * mod * E_mod_age +
		agebin * sex * mod * E_mod_sex_age
	})
	spag_plot_data = within(spag_plot_data, { 
		mod_fitted_simple_slopes = (49.6 + sex * E_sex + 
		sex * mod * E_mod_sex) +
		agebin * (E_age + sex * agebin * E_sex_age)
		sex * E_sex + 
		sex * mod * E_mod_sex +
		agebin * E_age + 
		agebin * sex * E_sex_age
	})
	
	spag_plot_data = within(spag_plot_data, { 
		mod_fitted = 49.6 + 
		sex * E_sex + 
		sex * mod * E_mod_sex +
		agebin * E_age + 
		agebin * sex * E_sex_age + 
		agebin * mod * E_mod_age +
		agebin * sex * mod * E_mod_sex_age
	})
	spag_plot_data[, c("age","Gender", "country", mod, zmod, "mod_fitted_simple_intercept", "mod_main", "mod_fitted_simple_slopes", "mod_fitted")]	
}

karo_theme = theme_bw(base_size=12)  + 
	theme(
		plot.title = element_text(size = 20),
		axis.title.x = element_text(size = 26, vjust=-.2),
		axis.title.y = element_text(size = 26, vjust=1),
		axis.text = element_text(size = 22, vjust=1))

load(file = "fitted_effects.rdata")


shinyServer(function(input, output, session) {
		
	observe({
		updateCheckboxGroupInput(
			session, 'countries', choices = countries, inline = T,
			selected = ({if (input$all_countries > 0) {
				if (input$all_countries %% 2 == 0) {
					c()
				} else {
					countries
				}
			} else c("United States", "Singapore", "Argentina", "India")
			})
		)
	})
	getAnnot = reactive({
		annot = unique(spag_plot_data[, c("Gender","country","age")])
		gender_order = rnorm(nrow(annot)/2)
		annot$rand = NA
		annot[annot$Gender=="male",]$rand =	gender_order
		annot[annot$Gender=="female",]$rand =	gender_order
		annot = annot[order(annot$rand, annot$Gender), ]
		annot = annot[!duplicated(data.frame(annot$country, annot$Gender)), ]
		annot
	})
	getSlopes = reactive({ 
		get_fitted(spag_plot_data = spag_plot_data, mplusfits = mplusfits, mod = input$moderator) 
	})
	
	
  output$agePlot <- renderPlot({
  	annot = getAnnot()
  	
  	mod = input$moderator
  	zmod = paste0('z',input$moderator)
  	longmod = mplusfits[mplusfits$indicator_short == mod, "Indicator"]
  	
  	title = paste0("Age x Sex as moderated by ",longmod)
  	moderator = unique(spag_plot_data[,c('country',mod,zmod)])
  	moderator$normal = moderator[,mod]
  	moderator$rounded = round(moderator[, zmod],1)
  	length(unique(moderator$rounded))
  	mod_labels = moderator %>% group_by(rounded) %>% summarise(countries = paste(country, collapse = ", "), normal = mean(normal))
  	mod_limits = c(min(floor(moderator[,zmod]*10)/10,na.rm=T)-0.1,max(ceiling(moderator[,zmod]*10)/10, na.rm=T)+0.1)
  	
  	if(input$fitted) {
  		slopes = getSlopes()
  		slopes = slopes[slopes$country %in% input$countries, ]
  		slopes[, zmod] = round(slopes[, zmod],1)
  		
  		
  		outcome = ifelse(input$moderated_intercepts, "mod_fitted","mod_fitted_simple_intercept")
  		
  		annot2 = merge(annot, slopes[, c("country","Gender","age",zmod,outcome)], by = c("country","Gender","age"), all = F)
  		annot2[, outcome] = annot2[, outcome] + 0.1
  		
  		spaghetti_fitted = ggplot(data = slopes) + 
	  	 	geom_line(aes_string(x = "age",y = outcome, group = "interaction(country,Gender)", linetype = "Gender", colour = zmod)) + 
  			scale_x_continuous("Age", breaks = seq(16,45,by=5)) + 
  			scale_y_continuous("Self esteem") +
  			karo_theme +
	  	 	guides(linetype = guide_legend(order = 1)) +
	  	 	ggtitle(title) +
	  	 scale_colour_continuous(mod, guide = guide_colourbar(barheight=35),high = "#89CFF0", low = "pink", space = "Lab",breaks= mod_labels$rounded,labels=mod_labels$countries, limits = mod_limits)
  		if(input$annotate_countries) {
				spaghetti_fitted + geom_text(aes_string(x = "age", y = outcome, colour = zmod, label = "country", group = "Gender"), data = annot2, angle = 10)
  		} else {
  			spaghetti_fitted
  		}
  	} else { # smoothed raw data
  		
  		age_trends[, zmod] = round(age_trends[, zmod],1)
  		age_trends = age_trends[age_trends$country %in% input$countries, ]
  		age_trends$age = age_trends$x
  		age_trends$T_self = age_trends$y
  		label_places = age_trends[, c("country","Gender","age",zmod,"T_self")]
  		label_places$age = round(label_places$age)
  		label_places = label_places[ !duplicated(data.frame(label_places$country, label_places$Gender, label_places$age)), ]
  		annot2 = merge(annot, label_places, by = c("country","Gender","age"), all = F)
#   		tail(age_trends[, c("country","Gender","age")])
#   		tail(annot[, c("country","Gender","age")])
  		annot2$T_self = annot2$T_self + 0.1
  		
  		spaghetti_raw <- ggplot(age_trends,
  														aes_string(x = "age", y = "T_self", colour = zmod, group = "interaction(Gender, country)")) + 
  			scale_linetype_discrete() +
  			scale_fill_continuous(guide=F) +
  			scale_x_continuous("Age", breaks = seq(16,45,by=5)) + 
  			scale_y_continuous("Self esteem") +
  			karo_theme +
  			guides(linetype = guide_legend(order = 1)) +
  			ggtitle(title) +
  			scale_colour_continuous(mod, guide = guide_colourbar(barheight=35),high = "#89CFF0", low = "pink", space = "Lab",breaks= mod_labels$rounded,labels=mod_labels$countries, limits = mod_limits)
  		
  		
  		if(input$standard_errors) {
#   			spaghetti_raw = spaghetti_raw + geom_smooth(method = "lm", aes(linetype = Gender), alpha=0.1, size = 1)
  				spaghetti_raw = spaghetti_raw + geom_smooth(stat = "identity", aes(ymax = ymax, ymin = ymin, linetype = Gender), alpha=0.1, size = 1)
  		} else {
  			spaghetti_raw = spaghetti_raw + geom_smooth(method = "lm", aes( linetype = Gender), alpha=0.1, size = 1, se = F)
				spaghetti_raw = spaghetti_raw + geom_line(aes(ymax = ymax, ymin = ymin, linetype = Gender), alpha = 1, size = 1)
  		}
  		if(input$annotate_countries) {
  			spaghetti_raw + geom_text(aes_string(x = "age", y = "T_self", colour = zmod, label = "country", group = "Gender"), data = annot2)
  		} else {
  			spaghetti_raw
  		}
  	}
#   	else {
#   		
#   		
#   		age_trends = age_trends
#   		age_trends[, zmod] = round(age_trends[, zmod],1)
#   		age_trends = age_trends[age_trends$country %in% input$countries, ]
# #   		annot$age_r = round(annot$age/3)*3
#   		annot2 = merge(annot, age_trends[, c("country","Gender","age",zmod,"T_self","T_self_no_nation")], by = c("country","Gender","age"), all = F)
#   		annot2$T_self = annot2$T_self + 0.1
# 
# 	  	spaghetti_raw <- ggplot(age_trends,
# 	  			aes_string(x = "age", y = "T_self", colour = zmod, group = "interaction(Gender, country)")) + 
# 					scale_linetype_discrete() +
# 					scale_fill_continuous(guide=F) +
# 					scale_x_continuous("Age", breaks = seq(16,45,by=5)) + 
# 					scale_y_continuous("Self esteem") +
# 					karo_theme +
# 					guides(linetype = guide_legend(order = 1)) +
# 					ggtitle(title) +
# 					scale_colour_continuous(mod, guide = guide_colourbar(barheight=35),high = "#89CFF0", low = "pink", space = "Lab",breaks= mod_labels$rounded,labels=mod_labels$countries, limits = mod_limits)
# 	
# 	
# 	  	if(input$standard_errors) {
# 	  		spaghetti_raw = spaghetti_raw + geom_smooth(method = "lm", aes(linetype = Gender), alpha=0.1, size = 1)
# # 	  		spaghetti_raw = spaghetti_raw + geom_smooth(stat = "identity", aes(ymax = ucl, ymin = lcl, linetype = Gender), alpha=0.1, size = 1)
# 	  	} else {
# 	  		spaghetti_raw = spaghetti_raw + geom_smooth(method = "lm", aes( linetype = Gender), alpha=0.1, size = 1, se = F)
# # 	  		spaghetti_raw = spaghetti_raw + geom_line(aes(ymax = ucl, ymin = lcl, linetype = Gender), alpha=1, size = 1)
# 	  	}
#   		if(input$annotate_countries) {
#   			spaghetti_raw + geom_text(aes_string(x = "age", y = "T_self", colour = zmod, label = "country", group = "Gender"), data = annot2)
#   		} else {
#   			spaghetti_raw
#   		}
#   	}
  },width = "auto", height = 700)

})
