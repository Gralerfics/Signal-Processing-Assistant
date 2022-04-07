% By Gralerfics
classdef TS
    properties                                                              % Properties.
        l       % Left border.
        r       % Right border. [l, r)
        fs      % Sampling frequency, = 1 indicates discrete-time.
        value   % Signal value, length = (r - l) * fs.
    end
    
    methods (Access = private)                                              % Inline methods.
        % Fs equality checker.
        function flag = checkFs(A, B)
            if class(A) ~= "TS" || class(B) ~= "TS"
                error("'A' and 'B' should be time signals(TS).");
            end
            flag = (A.fs == B.fs);
        end

        % Elementary operation, func = @(a, b) a ? b.
        function y = elementaryOperation(A, B, func)
            if class(A) ~= "TS" || class(B) ~= "TS"
                error("'A' and 'B' should be time signals(TS).");
            end
            if ~checkFs(A, B)
                error("'A' and 'B' should have the same sampling frequency(fs).");
            end
            L = min(A.l, B.l);
            R = max(A.r, B.r);
            yValue = func(A.cut({L, R}).value, B.cut({L, R}).value);
            y = TS({L, R}, yValue, A.fs);
        end

        % Scalar convertor. mA and mB will not be both doubles.
        function [A, B] = convertScalar(mA, mB)
            if class(mA) == "double"
                A = TS({mB.l, mB.r}, mA, mB.fs);
            else
                A = mA;
            end
            if class(mB) == "double"
                B = TS({mA.l, mA.r}, mB, mA.fs);
            else
                B = mB;
            end
        end

    end

    methods                                                                 % Member methods.
        %%% Construction methods.
            % TS({l, r}, value, [fs]) - All parameters.
                % value can be a scalar.
            % TS({l}, value, [fs])    - Automatically generate r.
            % TS(value, [fs])         - Default l = 0.
            % TS(ts)                  - Copy.
        function obj = TS(varargin)
            narginchk(1, 3);
            if class(varargin{1}) == "cell"
                narginchk(2, 3);
                if nargin == 3
                    obj.fs = varargin{3};
                else
                    obj.fs = 1;
                end
                if length(varargin{2}) == 1 && length(varargin{1}) == 2
                    obj.value = varargin{2} * ones(1, (varargin{1}{2} - varargin{1}{1}) * obj.fs);
                else
                    obj.value = varargin{2};
                end
                obj.l = varargin{1}{1};
                if length(varargin{1}) == 2
                    obj.r = varargin{1}{2};
                    if obj.r < obj.l
                        error("'l' should be smaller than 'r'.");
                    end
                else
                    obj.r = obj.l + length(obj.value) / obj.fs;
                end
                if length(obj.value) ~= (obj.r - obj.l) * obj.fs
                    error("Wrong length of 'value'");
                end
            elseif class(varargin{1}) == "TS"
                narginchk(1, 1);
                obj = varargin{1};
            elseif class(varargin{1}) == "double"
                narginchk(1, 2);
                obj = TS({0}, varargin{:});
            end
        end

        %%% Getters and setters.
        function obj = set.l(obj, mL)
            if fix(mL) == mL
                obj.l = mL;
            else
                error("'l' should be a integer.");
            end
        end
        function obj = set.r(obj, mR)
            if fix(mR) == mR
                obj.r = mR;
            else
                error("'r' should be a integer.");
            end
        end
        function obj = set.fs(obj, mFs)
            if fix(mFs) == mFs && mFs > 0
                obj.fs = mFs;
            else
                error("'fs' should be a positive integer.");
            end
        end

        %%% Domain operations.
        % cut({l, r}) - [l, r), the part out of range will be filled
        %               with 0.
        function y = cut(obj, mLR)
            if length(mLR) ~= 2 || class(mLR) ~= "cell"
                error("'mLR' should be a cell array contains l and r.");
            end
            lo = (mLR{1} - obj.l) * obj.fs + 1;
            ro = (mLR{2} - obj.l) * obj.fs;
            n = length(obj.value);
            yValue = obj.value(max(1, lo) : min(n, ro));
            yValue = [zeros(1, max(0, 1 - lo)), yValue, zeros(1, max(0, ro - n))];
            y = TS({mLR{1}, mLR{2}}, yValue, obj.fs);
        end
        % shift(t0) - x(t) -> x(t - t0). t0 is a integer.
        function y = shift(obj, mT)
            if fix(mT) ~= mT
                error("'t0' should be a integer.");
            end
            y = obj;
            y.l = y.l - mT;
            y.r = y.r - mT;
        end
        % lerpShift(t0) - t0 is real.
        function y = lerpShift(obj, mT)
            % TODO
        end
        % lerpFs(newFs) - change sampling frequency with interpolation.
        function y = lerpFs(obj, mFs)
            % TODO
        end

        %%% Operations.
        function y = plus(mA, mB)                                           % +  : Addition
            [A, B] = convertScalar(mA, mB);
            y = elementaryOperation(A, B, @(a, b) a + b);
        end
        function y = minus(mA, mB)                                          % -  : Subtraction
            [A, B] = convertScalar(mA, mB);
            y = elementaryOperation(A, B, @(a, b) a - b);
        end
        function y = uminus(mA)                                             % -  : Opposite
            y = mA;
            y.value = -y.value;
        end
        function y = times(mA, mB)                                          % .* : Multiplication
            [A, B] = convertScalar(mA, mB);
            y = elementaryOperation(A, B, @(a, b) a .* b);
        end
        function y = mtimes(mA, mB)                                         % *  : Convolution
            if class(mA) == "double" || class(mB) == "double"
                y = mA .* mB;
            end
            y = TS.Convolution(mA, mB);
        end
        function y = rdivide(mA, mB)                                        % ./ : Right division
            [A, B] = convertScalar(mA, mB);
            y = elementaryOperation(A, B, @(a, b) a ./ b);
        end
        function y = power(mA, mB)                                          % .^ : Power
            [A, B] = convertScalar(mA, mB);
            y = elementaryOperation(A, B, @(a, b) a .^ b);
        end
        function y = mpower(mA, mB)                                         % ^  : Multi-convolution
            if class(mB) ~= "double" || fix(mB) ~= mB || B < 1
                error("'B' should be a positive integer.");
            end
            y = mA;
            for I = 2 : mB
                y = y * mA;
            end
        end
        function y = uniFunc(obj, name)                                     % func(s)
            y = obj;
            y.value = eval(name + "(y.value)");
        end
        function y = diaFunc(obj, name, arg)                                % func(s, arg0)
            y = obj;
            y.value = eval(name + "(y.value, arg)");
        end
    end

    methods (Static)                                                        % Static methods.
        %%% Specific signals
        % Identity({l, r}, [fs]) - y = t, y = n.
        function y = Identity(varargin)
            if nargin == 2; yFs = varargin{2}; else; yFs = 1; end
            if length(varargin{1}) ~= 2
                error("'mLR' should be a cell array contains l and r.");
            end
            mLR = varargin{1};
            yValueT = linspace(mLR{1}, mLR{2}, (mLR{2} - mLR{1}) * yFs + 1);
            y = TS(mLR, yValueT(1 : end - 1), yFs);
        end
        % Step({l, r}, [fs]) - y = u(t), y = u[n].
        function y = Step(varargin)
            if nargin == 2; yFs = varargin{2}; else; yFs = 1; end
            if length(varargin{1}) ~= 2
                error("'mLR' should be a cell array contains l and r.");
            end
            mLR = varargin{1};
            y = TS(mLR, 1, yFs).cut({min(0, mLR{2}), mLR{2}}).cut(mLR);
        end
        % Impulse({l, r}, [fs]) - y = δ(t), y = δ[n].
        function y = Impulse(varargin)
            if nargin == 2; yFs = varargin{2}; else; yFs = 1; end
            if length(varargin{1}) ~= 2
                error("'mLR' should be a cell array contains l and r.");
            end
            mLR = varargin{1};
            y = TS(mLR, 0, yFs);
            p = 1 - mLR{1} * yFs;
            if p >= 1 && p <= length(y.value)
                y.value(p) = yFs;
            end
        end

        %%% Operations
        % Convolution(mA, mB) - mA * mB. Remenber ./ fs.
        function y = Convolution(mA, mB)
            if class(mA) ~= "TS" || class(mB) ~= "TS"
                error("'A' and 'B' should be time signals(TS).");
            end
            if ~checkFs(mA, mB)
                error("'A' and 'B' should have the same sampling frequency(fs).");
            end
            y = TS({mA.l + mB.l, mA.r + mB.r}, [conv(mA.value, mB.value), 0] ./ mA.fs, mA.fs);
        end
    end
end
