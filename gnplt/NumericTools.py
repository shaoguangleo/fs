#
# Copyright (c) 2020 NVI, Inc.
#
# This file is part of VLBI Field System
# (see http://github.com/nvi-inc/fs).
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#
from numpy import *
from numpy.random import *
from numpy.linalg import solve, lstsq, inv
from GnPltError import *
import datetime, time

class NumericTools:
    """NumericTools contains all numeric tools used by gnplot.
    It uses NumPy for a major part of the computations.
    """

    def date2num(self, timestr):
        #cut the milliseconds
        timestr = timestr[:-3]
        dt = time.strptime(timestr, "%Y.%j.%H:%M:%S")
        timenum = time.mktime(dt)
        return float(timenum)

    def num2date(self, timenum):
        date = datetime.datetime.fromtimestamp(timenum)
        date_formatted = datetime.datetime.strftime(date, "%Y.%j.%H:%M:%S")
        return date_formatted

    def getTimeDiff(self, timenum1, timenum2):
        """receives two num objects, time1 and time2, and returns the time difference in hours
        """
        dtime1 = datetime.datetime.fromtimestamp(timenum1)
        dtime2 = datetime.datetime.fromtimestamp(timenum2)
        tdelta = dtime1-dtime2
        thours = tdelta.days*24 + tdelta.seconds/3600.0
        return thours

    def polyfitData(self, x_list, y_list, degree):
        poly = polyfit(x_list, y_list, degree)
        poly = list(poly)
        return poly

    def evalPoly(self, poly, x):
        x = array(x)
        y = polyval(poly, x)
        #print('NumTools')
        return list(y)

    def getRMS(self, poly, x, y):
        m = len(x)
        x = array(x)
        y = array(y)
        yprime = polyval(poly, x)
        error_squared = (yprime-y)**2
        RMS = sqrt(sum(error_squared)/m)
        return RMS

    def getDPFU(self, poly):
        #find max
        x = arange(0,90,0.5)
        y = self.evalPoly(poly, x)
        DPFU = max(y)
        return DPFU

    def getMedian(self, list):
        return median(list)

    def getMean(self, list):
        return float(sum(list))/len(list)

    def getScale(self, y, x):
        return sum(array(y)/array(x))/float(len(x))

    def getList(self, name, indices):
        """not used"""
        return_data = []
        for i in indices:
            return_data.append(self.database.get(name)[i])
        return return_data

    def solveForTrec(self, x, y, Tatm):
        #the y-data is Tsys-Tspill
        #x data is Airmass
        #k1 is Trec
        #k2 is zenith opacity
        #we want to find k1, k2
        #m is length
        m = len(y)
        y = array(y)
        x = array(x)
        #initial guesses:
        k2 = 0
        k1 = 14
        #iterate over k2, k1 until dbeta ---> zero
        t=0
        conv_crit = 1.0
        MAX_ITERATIONS = 15
        CONVERGENCE_CRITERIUM = 1e-10
        while (conv_crit > CONVERGENCE_CRITERIUM):
            t+= 1
            f = k1 + Tatm*(1-exp(-k2*x))
            dfdk1 = [1]*m
            dfdk2 = Tatm*x*exp(-k2*x)
            dBeta = array(y - f)

            #construct A
            A = array(zeros((m,2), dtype = float))
            for i in range(m):
                A[i][0] = dfdk1[i]
                A[i][1] = dfdk2[i]

            #solve system
            lstsq_output = linalg.lstsq(A,dBeta,rcond=-1)
            [dk1, dk2]= lstsq_output[0]

            k1 += dk1
            k2 += dk2

            if (t>MAX_ITERATIONS):
                raise NonConvergenceError

            #compute sigmas of parameters:

            E = inv(dot(A.T,A))
            e1 = E[0][0]
            e2 = E[1][1]

            conv_crit = max(abs(dk1/sqrt(e1)), abs(dk2/sqrt(e2)))

        S = dot(dBeta,dBeta)
        factor = m - 2
        sigma1 = sqrt(S/factor*e1)
        sigma2 = sqrt(S/factor*e2)

        return [k1, k2, sigma1, sigma2, m]

    def getOpacity(self, tsys_tspill, trec, tatm):
        k = array(tsys_tspill)#self.getMean(tsys_tspill)
        exptau = (trec-k)/tatm+1
        return exptau

    def getMaxGain(self, polyList):
        d_polyList = []
        dd_polyList = []
        polyListRev = []
        polyListRev.extend(polyList)
        polyListRev.reverse()
        for i in range(1, len(polyList)):
            d_polyList.append(i * polyListRev[i])
        for i in range(1, len(d_polyList)):
            dd_polyList.append(i*d_polyList[i])

        d_polyList.reverse()
        dd_polyList.reverse()
        x = 50.0
        x, f = self.newtonRaphson(x, polyList, d_polyList, dd_polyList)
        return x, f

    def newtonRaphson(self, x, polyList, d_polyList, dd_polyList, TOL=1e-13):
        count = 0
        while 1:
            f = self.evalPoly2(polyList, x)
            fd = self.evalPoly2(d_polyList, x)
            fdd = self.evalPoly2(dd_polyList, x)
            dx = fd / float(fdd)
            if abs(dx) < TOL :
                x = x - dx
                return x , f
            x = x - dx
            count = count + 1

    def evalPoly2(self, poly, x):
        y = 0
        m = len(poly)-1
        for k in range(len(poly)):
            y += poly[k]*x**(m-k)
        return y


def main():
    num = NumericTools()
    t = num.date2num('2008.176.11:50:45.23')
    print(t)
    print(num.num2date(t))



if __name__ == '__main__':
    main()
