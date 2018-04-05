# R
The case:

Your team has been engaged to develop a Customer Churn model. Your client is a large telecommunications company facing intense competition from industry rivals due to a number of structural factors in the industry. The most important of these factors are:

	A market approaching saturation
	Low switching costs
	Little differentiation in the product and service
	Credible threats to forward integration from suppliers
	Low barriers to entry and high barriers to exit
The client is therefore forced to defend its existing market share aggressively, and has just completed an internal project to design a retention offer for at-risk customers. Some of the findings from this project, which your team must take as given, are as follows:

	The percentage of at-risk customers in the current customer base is 26.448% (this number is supported by the data you will be provided).
	The retention offer will cost the company $1,600 per customer (one-time)
	A lost customer costs the company $11,500 (one time)
	The project has concluded through detailed customer surveys and historical data analysis that the retention offer will be 45% effective. That is, 45% of the customers who have been identified as at-risk and who receive the offer will change their minds and remain with the company. The remaining 55% will leave anyway.

The objective of your engagement is to develop a predictive model to identify the customers who should receive the retention offer, and to evaluate the resulting economic consequences. This model will be used on an ongoing basis to make this decision about future customers. Accordingly, you will be provided with a dataset containing usage information for a random sample of their current and past customers, and an indication of whether or not they have left the company.

Your model must do the following:

	Predict which customers are at-risk given their usage history and should therefore receive the retention offer.
	Identify the associated “expected cost per customer,” taking into account the probabilities of both Type I and Type II errors, and the probability that the retention offer will be effective in preventing only 45% of at-risk customer from leaving.
		Note that this expected cost per customer should be less than
			-The “do-nothing” option, which would cost $0.00 per customer in retention offer cost but incur a lost-customer cost of $3,041.52 (26.448% * $11,500)
			-The “offer-to-all” option, which would cost $1,600 per customer in retention offer costs plus 26.448% * 55% * $11,500 = $1,672.84 per customer in lost-customer costs, totaling $3,272.84 per customer
