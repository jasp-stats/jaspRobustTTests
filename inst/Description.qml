import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name		: "jaspRobustTTests"
	title		: qsTr("Robust T-Tests")
	description	: qsTr("Robustly evaluate the difference between two means")
	version			: "0.19.2"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"
	icon		: "analysis-bayesian-ttest.svg"
	hasWrappers	: false

	Analysis
	{
		menu:	qsTr("Model-Averaged T-Test")
		title:	qsTr("Bayesian Model-Averaged T-Test")
		func:	"BayesianModelAveragedTTest"
	}

	Analysis
	{
		menu:	qsTr("Robust Model-Averaged T-Test")
		title:	qsTr("Robust Bayesian Model-Averaged T-Test")
		func:	"RobustBayesianModelAveragedTTest"
	}

	Analysis
	{
		menu:	qsTr("Truncated Model-Averaged T-Test")
		title:	qsTr("Truncated Bayesian Model-Averaged T-Test")
		func:	"TruncatedBayesianModelAveragedTTest"
	}
}
