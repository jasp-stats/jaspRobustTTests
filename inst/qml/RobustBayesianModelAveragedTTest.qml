//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls
import "./common" as Common

Form {
	id: form
	property int framework:	Common.Type.Framework.Bayesian

	plotHeight: 340
	plotWidth:  420

	Common.ModelAveragedTTestInput{}

	//// Inference ////
	Common.ModelAveragedTTestInference{}

	//// Plots section ////
	Common.ModelAveragedTTestPlots{}

	//// Diagnostics section ////
	Common.ModelAveragedTTestMcmcDiagnostics{}

	//// Priors ////
	Section
	{
		title: 				qsTr("Models")
		columns:			1


		// effect prior
		Common.ModelAveragedTTestPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsEffect"
		}

		// heterogeneity prior
		Common.ModelAveragedTTestPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsUnequalVariances"
		}

		// df priors
		Common.ModelAveragedTTestPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsOutliers"
		}

		Divider { }

		CheckBox
		{
			id:						priorsNull
			name:					"priorsNull"
			label:					qsTr("Set null priors")
		}

		// effect prior
		Common.ModelAveragedTTestPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsEffectNull"
			visible:				priorsNull.checked
		}

		// heterogeneity prior
		Common.ModelAveragedTTestPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsUnequalVariancesNull"
			visible:				priorsNull.checked
		}

		// df priors
		Common.ModelAveragedTTestPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsOutliersNull"
			visible:				priorsNull.checked
		}

	}

	//// Advanced section for prior model probabilities sampling settings ////
	Common.ModelAveragedTTestAdvanced{}

}
