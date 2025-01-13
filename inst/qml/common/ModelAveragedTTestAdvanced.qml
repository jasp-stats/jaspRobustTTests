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
import JASP.Controls
import JASP

Section
{
	title: 				qsTr("Advanced")
	columns: 			2

	Group
	{
		rowSpacing: 10 * preferencesModel.uiScale

		Group
		{
			title: 		qsTr("MCMC")

			IntegerField
			{
				name:			"advancedMcmcAdaptation"
				label:			qsTr("Adaptation")
				defaultValue:	5000
				min:			1000
				fieldWidth:		55 * preferencesModel.uiScale
			}
			IntegerField
			{
				name:			"advancedMcmcSamples"
				label:			qsTr("Samples")
				defaultValue:	5000
				min:			4000
				fieldWidth:		55 * preferencesModel.uiScale
			}
			IntegerField
			{
				name:			"advancedMcmcChains"
				label:			qsTr("Chains")
				defaultValue:	4
				min:			1
				fieldWidth:		55 * preferencesModel.uiScale
			}
			IntegerField
			{
				name:			"advancedMcmcThin"
				label:			qsTr("Thin")
				defaultValue:	1
				min:			1
				fieldWidth:		55 * preferencesModel.uiScale
			}

		}

		SetSeed{}
	}
}
